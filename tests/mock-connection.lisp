;;;; tests/mock-connection.lisp
;;;;
;;;; A stand-in browser connection for CLOG, and a clog-port built on it.
;;;;
;;;; The connection is a socket object registered in CLOG's connection
;;;; tables under a fresh id.  It records every script CLOG sends to it,
;;;; one script per websocket message, and answers each query at once with
;;;; a fixed numeric string, so text measurement and property reads never
;;;; wait on a browser.  Its WEBSOCKET-DRIVER:SEND method specializes only
;;;; the class defined here; registering and releasing it add and remove
;;;; only the table entries for its own id.

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
           #:deliver-browser-message))

(in-package #:clim-clog/tests/mock-connection)

(defparameter *query-answer* "10"
  "The answer the mock socket gives to every query script.")

(defclass mock-socket ()
  ((id :initarg :id :reader mock-socket-id)
   (scripts :initform '() :accessor %scripts
            :documentation "The scripts sent to the socket, newest first.")
   (lock :initform (clim-sys:make-lock "mock socket") :reader %lock))
  (:documentation "A websocket stand-in that records the scripts CLOG sends
and answers CLOG queries without a browser."))

(defun %query-id (script)
  "The integer id of the CLOG query SCRIPT carries, or NIL when SCRIPT is
not a query.  CLOG wraps a query as ws.send (\"ID:\"+eval(\"...\"))."
  (let ((prefix "ws.send (\""))
    (when (and (> (length script) (length prefix))
               (string= prefix script :end2 (length prefix)))
      (parse-integer script :start (length prefix) :junk-allowed t))))

(defmethod websocket-driver:send ((ws mock-socket) data
                                  &key start end type code callback)
  "Record DATA, and answer it at once when it is a CLOG query."
  (declare (ignore start end type code callback))
  (clim-sys:with-lock-held ((%lock ws))
    (push data (%scripts ws)))
  (let ((query-id (%query-id data)))
    (when query-id
      (let ((semaphore (gethash query-id clog-connection::*queries-sems*)))
        (when semaphore
          (setf (gethash query-id clog-connection::*queries*) *query-answer*)
          (bordeaux-threads:signal-semaphore semaphore)))))
  nil)

(defun make-mock-socket ()
  "Register a fresh mock socket in CLOG's connection tables and return it."
  (let* ((id (format nil "mock-~a" (random most-positive-fixnum)))
         (socket (make-instance 'mock-socket :id id))
         (data (make-hash-table :test #'equal)))
    (setf (gethash "connection-id" data) id
          (gethash socket clog-connection::*connections*) id
          (gethash id clog-connection::*connection-ids*) socket
          (gethash id clog-connection::*connection-data*) data)
    socket))

(defun release-mock-socket (socket)
  "Remove SOCKET's entries from CLOG's connection tables."
  (let ((id (mock-socket-id socket)))
    (remhash socket clog-connection::*connections*)
    (remhash id clog-connection::*connection-ids*)
    (remhash id clog-connection::*connection-data*))
  (values))

(defun sent-scripts (socket)
  "The scripts sent to SOCKET, oldest first."
  (clim-sys:with-lock-held ((%lock socket))
    (reverse (%scripts socket))))

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
  "Hand MESSAGE to CLOG as though it arrived on SOCKET's websocket."
  (clog-connection::handle-message socket message))
