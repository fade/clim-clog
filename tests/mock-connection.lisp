;;;; tests/mock-connection.lisp
;;;;
;;;; A stand-in browser connection for CLOG, and a clog-port built on it.
;;;;
;;;; The connection is a socket object registered in CLOG's connection
;;;; tables under a fresh id.  It records every script CLOG sends to it,
;;;; one script per websocket message, and answers each query with a fixed
;;;; numeric string.  As with a browser, the answer comes back as an
;;;; "ID:answer" websocket message: a reader thread of the socket's own hands
;;;; it to CLOG-CONNECTION::HANDLE-MESSAGE, in order with the messages
;;;; DELIVER-BROWSER-MESSAGE sends.  A handler that queries while running on
;;;; that thread therefore gets no answer until it returns, and waits out
;;;; CLOG-CONNECTION::*QUERY-TIME-OUT*.  Its WEBSOCKET-DRIVER:SEND method
;;;; specializes only the class defined here; registering and releasing it
;;;; add and remove only the table entries for its own id.

(defpackage #:clim-clog/tests/mock-connection
  (:use #:clim #:clim-lisp)
  (:export #:mock-socket
           #:mock-socket-id
           #:make-mock-socket
           #:release-mock-socket
           #:sent-scripts
           #:clear-sent-scripts
           #:blit-count
           #:make-mock-port
           #:release-mock-port
           #:stop-port-event-loop
           #:deliver-browser-message
           #:mock-socket-reader
           #:reader-thread-queries))

(in-package #:clim-clog/tests/mock-connection)

(defparameter *query-answer* "10"
  "The answer the mock socket gives to every query script.")

(defparameter *delivery-timeout* 30
  "Seconds DELIVER-BROWSER-MESSAGE waits for the reader thread to handle a
message before it signals an error.")

(defclass mock-socket ()
  ((id :initarg :id :reader mock-socket-id)
   (scripts :initform '() :accessor %scripts
            :documentation "The scripts sent to the socket, newest first.")
   (inbox :initform '() :accessor %inbox
          :documentation "Messages the reader thread has yet to handle,
oldest first.  Each is a (MESSAGE . DONE) pair: MESSAGE is the websocket
text, or :STOP to end the thread, and DONE is a semaphore signalled once
MESSAGE is handled, or NIL.")
   (inbox-count :initform (bt:make-semaphore :name "mock socket inbox")
                :reader %inbox-count)
   (reader :initform nil :accessor mock-socket-reader
           :documentation "The thread that hands the inbox to CLOG.")
   (reader-queries :initform '() :accessor %reader-queries
                   :documentation "The queries sent from the reader
thread, newest first.")
   (lock :initform (clim-sys:make-lock "mock socket") :reader %lock))
  (:documentation "A websocket stand-in that records the scripts CLOG sends,
answers CLOG queries without a browser, and hands incoming messages to CLOG
on a reader thread of its own."))

(defun %query-id (script)
  "The integer id of the CLOG query SCRIPT carries, or NIL when SCRIPT is
not a query.  CLOG wraps a query as ws.send (\"ID:\"+eval(\"...\"))."
  (let ((prefix "ws.send (\""))
    (when (and (> (length script) (length prefix))
               (string= prefix script :end2 (length prefix)))
      (parse-integer script :start (length prefix) :junk-allowed t))))

(defun %post (socket message &optional done)
  "Append MESSAGE to SOCKET's inbox, with DONE to signal once it is handled."
  (clim-sys:with-lock-held ((%lock socket))
    (setf (%inbox socket)
          (nconc (%inbox socket) (list (cons message done)))))
  (bt:signal-semaphore (%inbox-count socket))
  (values))

(defun %reader-loop (socket)
  "Hand each message in SOCKET's inbox to CLOG-CONNECTION::HANDLE-MESSAGE,
oldest first, until the message :STOP.  Bind
CLOG-CONNECTION:*BREAK-ON-ERROR* to NIL, so an error in a handler is logged
rather than entering the debugger on this thread."
  (let ((clog-connection:*break-on-error* nil))
    (loop
      (bt:wait-on-semaphore (%inbox-count socket))
      (destructuring-bind (message . done)
          (clim-sys:with-lock-held ((%lock socket))
            (pop (%inbox socket)))
        (when (eq message :stop)
          (return))
        (unwind-protect (clog-connection::handle-message socket message)
          (when done
            (bt:signal-semaphore done)))))))

(defmethod websocket-driver:send ((ws mock-socket) data
                                  &key start end type code callback)
  "Record DATA.  When it is a CLOG query, post the answer to the socket's
reader thread as the message \"ID:answer\", and record DATA as a reader
thread query when sent from that thread."
  (declare (ignore start end type code callback))
  (let ((query-id (%query-id data)))
    (clim-sys:with-lock-held ((%lock ws))
      (push data (%scripts ws))
      (when (and query-id (eq (bt:current-thread) (mock-socket-reader ws)))
        (push data (%reader-queries ws))))
    (when query-id
      (%post ws (format nil "~d:~a" query-id *query-answer*))))
  nil)

(defun make-mock-socket ()
  "Register a fresh mock socket in CLOG's connection tables, start its
reader thread, and return it."
  (let* ((id (format nil "mock-~a" (random most-positive-fixnum)))
         (socket (make-instance 'mock-socket :id id))
         (data (make-hash-table :test #'equal)))
    (setf (gethash "connection-id" data) id
          (gethash socket clog-connection::*connections*) id
          (gethash id clog-connection::*connection-ids*) socket
          (gethash id clog-connection::*connection-data*) data
          (mock-socket-reader socket)
          (bt:make-thread (lambda () (%reader-loop socket))
                          :name (format nil "mock websocket reader ~a" id)))
    socket))

(defun release-mock-socket (socket)
  "Stop SOCKET's reader thread once it has handled every message already
posted, and remove SOCKET's entries from CLOG's connection tables.  Signal
an error when the thread is still running after *DELIVERY-TIMEOUT* seconds."
  (let ((reader (mock-socket-reader socket)))
    (when reader
      (%post socket :stop)
      (let ((deadline (+ (get-internal-real-time)
                         (* *delivery-timeout* internal-time-units-per-second))))
        (loop while (bt:thread-alive-p reader)
              do (when (> (get-internal-real-time) deadline)
                   (error "The mock reader thread of ~a did not stop within ~d s."
                          (mock-socket-id socket) *delivery-timeout*))
                 (sleep 0.01)))
      (bt:join-thread reader)
      (setf (mock-socket-reader socket) nil)))
  (let ((id (mock-socket-id socket)))
    (remhash socket clog-connection::*connections*)
    (remhash id clog-connection::*connection-ids*)
    (remhash id clog-connection::*connection-data*))
  (values))

(defun sent-scripts (socket)
  "The scripts sent to SOCKET, oldest first."
  (clim-sys:with-lock-held ((%lock socket))
    (reverse (%scripts socket))))

(defun reader-thread-queries (socket)
  "The queries sent to SOCKET from its reader thread, oldest first.  Each
one waits out the query timeout, as it does in a browser."
  (clim-sys:with-lock-held ((%lock socket))
    (reverse (%reader-queries socket))))

(defun clear-sent-scripts (socket)
  "Forget the scripts sent to SOCKET so far."
  (clim-sys:with-lock-held ((%lock socket))
    (setf (%scripts socket) '()))
  (values))

(defun blit-count (scripts)
  "The number of SCRIPTS that copy a canvas image (a drawImage call), which
is how a mirror presents its offscreen buffer."
  (count-if (lambda (script) (search ".drawImage(" script)) scripts))

(defun make-mock-port (socket &key (width 800) (height 600))
  "A clog-port whose body element lives on SOCKET's connection."
  (let ((body (make-instance 'clog:clog-body
                             :connection-id (mock-socket-id socket)
                             :html-id "body")))
    (make-instance 'clim-clog:clog-port
                   :server-path (list :clog :body body
                                      :width width :height height))))

(defun stop-port-event-loop (port)
  "Stop PORT's I/O loop so that events stay in the clog-port queue."
  (let ((process (climi::port-event-process port)))
    (when process
      (clim-sys:destroy-process process)
      (setf (climi::port-event-process port) nil)))
  (values))

(defun release-mock-port (port)
  "Stop PORT's I/O loop and drop it from the port registry."
  (ignore-errors (destroy-port port))
  (values))

(defun deliver-browser-message (socket message)
  "Hand MESSAGE to CLOG on SOCKET's reader thread, as though it arrived on
SOCKET's websocket, after every message already posted.  Return once CLOG
has handled it.  Signal an error when that takes longer than
*DELIVERY-TIMEOUT* seconds, as it does when called on the reader thread."
  (let ((done (bt:make-semaphore :name "mock socket delivery")))
    (%post socket message done)
    (unless (bt:wait-on-semaphore done :timeout *delivery-timeout*)
      (error "The mock reader thread of ~a did not handle ~s within ~d s."
             (mock-socket-id socket) message *delivery-timeout*)))
  (values))
