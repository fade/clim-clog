;;;; coalesced-flush.lisp
;;;;
;;;; Where coalescing scopes open and where they present early.
;;;;
;;;; Handling one event and redisplaying a frame or a pane each run in a
;;;; coalescing scope (see WITH-COALESCED-FLUSHES in port.lisp), so each
;;;; presents its mirror once, with the finished drawing, instead of once per
;;;; repaint.  A thread inside a scope that reads from a queue, or listens on
;;;; one with a timeout other than 0, first presents what the scope has drawn
;;;; so far, whether or not the call then blocks.  A menu or pointer-tracking
;;;; loop run from a handler therefore shows each step before it reads the
;;;; next event.  Each method only binds or flushes a per-thread set of clog
;;;; mirrors, so sheets of other ports behave as before.

(in-package #:clim-clog)

(defmethod handle-event :around ((sheet basic-sheet) event)
  (declare (ignore event))
  (with-coalesced-flushes ()
    (call-next-method)))

(defmethod redisplay-frame-pane :around ((frame application-frame) pane &key force-p)
  (declare (ignore pane force-p))
  (with-coalesced-flushes ()
    (call-next-method)))

(defmethod redisplay-frame-panes :around ((frame application-frame) &key force-p)
  (declare (ignore force-p))
  (with-coalesced-flushes ()
    (call-next-method)))

;;; Queue reads and listens present the scope's pending drawing first.
;;; QUEUE-READ presents on every call, including the read of an input
;;; buffer that STREAM-READ-GESTURE makes once STREAM-INPUT-WAIT has found a
;;; gesture, which does not block.  The two timed operations present unless
;;; the timeout is 0, which only polls.  Outside a scope none of them acts.

(defmethod climi::queue-read :before (queue)
  (declare (ignore queue))
  (flush-awaiting-mirrors))

(defmethod climi::queue-read-or-wait :before (queue timeout wait-function)
  (declare (ignore queue wait-function))
  (unless (and timeout (zerop timeout))
    (flush-awaiting-mirrors)))

(defmethod climi::queue-listen-or-wait :before (queue timeout wait-function)
  (declare (ignore queue wait-function))
  (unless (and timeout (zerop timeout))
    (flush-awaiting-mirrors)))
