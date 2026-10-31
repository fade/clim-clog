;;;; tests/input-order.lisp
;;;;
;;;; Contract tests for the order in which browser input reaches the CLIM
;;;; event queue.
;;;;
;;;; A canvas mirror lives on a mock connection (see mock-connection.lisp),
;;;; with the port's I/O loop stopped so that events stay in the clog-port
;;;; queue.  The ordering tests hand CLOG websocket messages for the canvas's
;;;; own events to CLOG's message entry point, as the websocket reader thread
;;;; does.  The queue must hold one event per message, in arrival order, by
;;;; the time each message has been handled: an event handled on a thread of
;;;; its own reaches the queue later, and in no fixed order.  A further test
;;;; calls CANVAS-MESSAGE-HANDLER directly to check which messages it leaves
;;;; to CLOG.

(defpackage #:clim-clog/tests/input-order
  (:use #:clim #:clim-lisp #:clim-backend
        #:clim-clog/tests/mock-connection)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:clim-clog/tests/input-order)

(defparameter *message-count* 50
  "Messages each ordering test delivers.")

(define-application-frame canvas-frame ()
  ()
  (:panes (canvas :application :scroll-bars nil))
  (:layouts (default canvas)))

(defun call-with-canvas (function)
  "Call FUNCTION with the mock socket, the port with its I/O loop stopped,
and the html-id of a realized canvas mirror, then tear them down."
  (let* ((socket (make-mock-socket))
         (port nil)
         (frame nil))
    (unwind-protect
         (progn
           (setf port (make-mock-port socket :width 400 :height 300)
                 frame (make-application-frame 'canvas-frame
                                               :frame-manager (find-frame-manager :port port)
                                               :width 400 :height 300))
           (enable-frame frame)
           (stop-port-event-loop port)
           (let* ((sheet (sheet-mirrored-ancestor (frame-top-level-sheet frame)))
                  (mirror (sheet-direct-mirror sheet)))
             (funcall function socket port
                      (clog:html-id (clim-clog::clog-mirror-canvas mirror)))))
      (when frame (ignore-errors (destroy-frame frame)))
      (when port (release-mock-port port))
      (release-mock-socket socket))))

(defun key-message (canvas-id key)
  "The websocket message a browser sends for a keydown of KEY on the canvas."
  (format nil "E:~a:keydown ~d:0:false:false:false:false:~a"
          canvas-id (char-code (char key 0)) key))

(defun wheel-message (canvas-id delta-y)
  "The websocket message a browser sends for a pixel-mode wheel turn of
DELTA-Y on the canvas."
  (format nil "E:~a:wheel 0:~d:0:0" canvas-id delta-y))

(defun queued-events (port)
  "The events in PORT's clog-port queue, oldest first."
  (clim-sys:with-lock-held ((clim-clog::clog-port-event-lock port))
    (copy-list (clim-clog::clog-port-event-queue port))))

(defun event-summary (event)
  "A comparable summary of EVENT: its key name, or its wheel delta."
  (typecase event
    (key-press-event (list :key (keyboard-event-key-name event)))
    (pointer-scroll-event (list :wheel (round (clime:pointer-event-delta-y event))))
    (t (list :other (type-of event)))))

(defun interleaved-messages (canvas-id)
  "*MESSAGE-COUNT* messages alternating keydowns and wheel turns, each paired
with the summary of the event it must queue."
  (loop for i below *message-count*
        collect (if (evenp i)
                    (let ((key (string (char "abcdefghijklmnopqrstuvwxyz"
                                             (mod (floor i 2) 26)))))
                      (cons (key-message canvas-id key)
                            (list :key (intern key :keyword))))
                    (cons (wheel-message canvas-id (1+ i))
                          (list :wheel (1+ i))))))

;;; ------------------------------------------------------------------
;;; Canvas events are queued on the thread that reads the websocket.
;;; ------------------------------------------------------------------

(define-test canvas-events-are-queued-before-the-message-returns
  (call-with-canvas
   (lambda (socket port canvas-id)
     (let ((lengths
             (loop for (message) in (interleaved-messages canvas-id)
                   do (deliver-browser-message socket message)
                   collect (length (queued-events port)))))
       (is equal (loop for n from 1 to *message-count* collect n) lengths)
       (is equal '() (reader-thread-queries socket))))))

(define-test canvas-events-keep-arrival-order
  (call-with-canvas
   (lambda (socket port canvas-id)
     (let ((messages (interleaved-messages canvas-id)))
       (dolist (message messages)
         (deliver-browser-message socket (car message)))
       (is equal (mapcar #'cdr messages)
           (mapcar #'event-summary (queued-events port)))
       (is equal '() (reader-thread-queries socket))))))

(define-test other-clog-events-keep-their-own-path
  (call-with-canvas
   (lambda (socket port canvas-id)
     (declare (ignore port))
     (let ((id (mock-socket-id socket)))
       (true (member 'clim-clog::canvas-message-handler
                     clog-connection::*message-handlers*))
       (false (clim-clog::canvas-message-handler
               (list "E" (format nil "~a:click 0" canvas-id)) id))
       (false (clim-clog::canvas-message-handler
               (list "E" "not-a-canvas:keydown 0") id))
       (false (clim-clog::canvas-message-handler (list "12" "answer") id))))))
