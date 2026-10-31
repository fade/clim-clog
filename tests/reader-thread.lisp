;;;; tests/reader-thread.lisp
;;;;
;;;; Contract tests for the handlers clim-clog runs on CLOG's websocket
;;;; reader thread.
;;;;
;;;; A probe object lives on a mock connection (see mock-connection.lisp)
;;;; whose reader thread delivers browser messages and query answers in
;;;; order, as a browser's websocket does.  The tests check that:
;;;;   - a query issued on that thread gets no answer until its handler
;;;;     returns;
;;;;   - an error in a handler is logged or reaches the debugger as
;;;;     CLOG-CONNECTION:*BREAK-ON-ERROR* directs, without the connection's
;;;;     "clog-debug" hook;
;;;;   - clim-clog's handler is on CLOG's list of message handlers.

(defpackage #:clim-clog/tests/reader-thread
  (:use #:clim-lisp
        #:clim-clog/tests/mock-connection)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false
                #:fail))

(in-package #:clim-clog/tests/reader-thread)

(defparameter *probe-query-time-out* 0.5
  "Seconds a probe handler waits for a query answer.")

(defun call-with-probe (function)
  "Call FUNCTION with a mock socket, a CLOG object on its connection, and
the connection's CLOG data, then release the socket."
  (let* ((socket (make-mock-socket))
         (id (mock-socket-id socket))
         (probe (make-instance 'clog:clog-obj :connection-id id
                                              :html-id "probe")))
    (unwind-protect
         (funcall function socket probe
                  (clog-connection:get-connection-data id))
      (release-mock-socket socket))))

(defun add-probe-handler (probe table event handler)
  "Bind HANDLER, a function of the event data, to EVENT on PROBE, and
register it to run on the reader thread.  Return the message body a browser sends for EVENT
with the data \"x\"."
  (let ((hook (format nil "~a:~a" (clog:html-id probe) event)))
    (setf (gethash hook table) handler)
    (clim-clog::register-reader-thread-hooks probe (list event))
    (format nil "~a x" hook)))

(defun seconds-since (start)
  "Seconds of real time elapsed since the internal real time START."
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

;;; ------------------------------------------------------------------
;;; A query on the reader thread waits for its own handler to return.
;;; ------------------------------------------------------------------

(define-test a-query-on-the-reader-thread-is-answered-after-its-handler
  (call-with-probe
   (lambda (socket probe table)
     (let (thread answer elapsed)
       (add-probe-handler
        probe table "query"
        (lambda (data)
          (declare (ignore data))
          (let ((clog-connection::*query-time-out* *probe-query-time-out*)
                (start (get-internal-real-time)))
            (setf thread (bt:current-thread)
                  answer (clog:js-query probe "1+1")
                  elapsed (seconds-since start)))))
       (deliver-browser-message socket (format nil "E:~a:query x"
                                                (clog:html-id probe)))
       (is eq (mock-socket-reader socket) thread)
       (false answer)
       (true (>= elapsed (* 0.9 *probe-query-time-out*)))
       (let ((clog-connection::*query-time-out* *probe-query-time-out*)
             (start (get-internal-real-time)))
         (is equal "10" (clog:js-query probe "1+1"))
         (true (< (seconds-since start) *probe-query-time-out*)))))))

;;; ------------------------------------------------------------------
;;; Errors are logged or reach the debugger, never the "clog-debug" hook.
;;; ------------------------------------------------------------------

(define-test an-error-is-logged-when-break-on-error-is-false
  (call-with-probe
   (lambda (socket probe table)
     (let* ((body (add-probe-handler
                   probe table "fail"
                   (lambda (data)
                     (declare (ignore data))
                     (error "probe handler failed"))))
            (log (make-string-output-stream))
            (handled (let ((clog-connection:*break-on-error* nil)
                           (*error-output* log))
                       (clim-clog::canvas-message-handler
                        (list "E" body) (mock-socket-id socket)))))
       (is eq t handled)
       (true (search "probe handler failed"
                     (get-output-stream-string log)))))))

(define-test the-debug-hook-is-not-called-on-the-reader-thread
  (call-with-probe
   (lambda (socket probe table)
     (let ((body (add-probe-handler
                  probe table "fail"
                  (lambda (data)
                    (declare (ignore data))
                    (error "probe handler failed"))))
           (hook-calls '())
           (seen '()))
       (setf (gethash "clog-debug" table)
             (lambda (event data)
               (push (list event data) hook-calls)
               (funcall event data)))
       (dolist (disabled '(nil t))
         (let ((clog-connection:*break-on-error* t)
               (clog-connection:*disable-clog-debugging* disabled)
               #+sbcl (sb-ext:*invoke-debugger-hook* nil)
               (*debugger-hook*
                 (lambda (condition hook)
                   (declare (ignore hook))
                   (push condition seen)
                   (invoke-restart (find-restart 'abort condition)))))
           (is eq t (clim-clog::canvas-message-handler
                     (list "E" body) (mock-socket-id socket)))))
       (is equal '() hook-calls)
       (is = 2 (length seen))
       (true (every (lambda (c)
                      (search "probe handler failed" (princ-to-string c)))
                    seen))))))

(define-test an-error-reaches-the-debugger-when-break-on-error-is-true
  (call-with-probe
   (lambda (socket probe table)
     (let ((body (add-probe-handler
                  probe table "fail"
                  (lambda (data)
                    (declare (ignore data))
                    (error "probe handler failed"))))
           (seen nil))
       (let ((clog-connection:*break-on-error* t)
             #+sbcl (sb-ext:*invoke-debugger-hook* nil)
             (*debugger-hook*
               (lambda (condition hook)
                 (declare (ignore hook))
                 (setf seen condition)
                 (invoke-restart (find-restart 'abort condition)))))
         (is eq t (clim-clog::canvas-message-handler
                   (list "E" body) (mock-socket-id socket))))
       (true (typep seen 'error))
       (true (search "probe handler failed" (princ-to-string seen)))))))

;;; ------------------------------------------------------------------
;;; CLOG's list of message handlers.
;;; ------------------------------------------------------------------

(define-test the-message-handler-list-is-checked
  (is eq 'clog-connection::*message-handlers*
      (clim-clog::clog-message-handlers-symbol))
  (true (member 'clim-clog::canvas-message-handler
                clog-connection::*message-handlers*))
  (fail (progv (list 'clog-connection::*message-handlers*) '()
          (clim-clog::clog-message-handlers-symbol))
      'error)
  (fail (let ((clog-connection::*message-handlers* :not-a-list))
          (clim-clog::clog-message-handlers-symbol))
      'error))
