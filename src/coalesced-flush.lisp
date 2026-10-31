;;;; coalesced-flush.lisp
;;;;
;;;; Where coalescing scopes open and where they present early.
;;;;
;;;; Handling one event, redisplaying a frame or a pane, and an input
;;;; editing session each run in a coalescing scope (see
;;;; WITH-COALESCED-FLUSHES in port.lisp), so their redraws reach the screen
;;;; as one frame with the finished drawing instead of once per repaint.  A
;;;; thread inside a scope that reads from a queue, or listens on one with a
;;;; timeout other than 0, first presents what the scope has drawn so far,
;;;; whether or not the call then blocks.  An input editor or a menu or
;;;; pointer-tracking loop run from a handler therefore shows each step as
;;;; it reads the next gesture.  Each method only binds or flushes a
;;;; per-thread set of clog mirrors, so sheets of other ports behave as
;;;; before.

(in-package #:clim-clog)

(defmethod handle-event :around ((sheet basic-sheet) event)
  (declare (ignore event))
  (with-coalesced-flushes ()
    (call-next-method)))

;;; McCLIM's own REDISPLAY-FRAME-PANE :AROUND specializes APPLICATION-FRAME
;;; and does the redisplay itself (clear history, record, repaint once).
;;; Specializing on STANDARD-APPLICATION-FRAME makes this method wrap it;
;;; the same specializers would replace it.

(defmethod redisplay-frame-pane :around
    ((frame standard-application-frame) pane &key force-p)
  (declare (ignore pane force-p))
  (with-coalesced-flushes ()
    (call-next-method)))

(defmethod redisplay-frame-panes :around
    ((frame standard-application-frame) &key force-p)
  (declare (ignore force-p))
  (with-coalesced-flushes ()
    (call-next-method)))

;;; An input editor echoes a gesture after HANDLE-EVENT has returned, from
;;; STREAM-READ-GESTURE, and redraws its buffer more than once per gesture.
;;; A scope over the whole editing session defers those redraws; the queue
;;; methods below present them once, as the next gesture is read or waited
;;; for, and the scope presents the last one on exit.  McCLIM has an
;;; :AROUND on all T and one on EXTENDED-OUTPUT-STREAM, so this method
;;; specializes CLIM-STREAM-PANE to wrap them.

(defmethod climi::invoke-with-input-editing :around
    ((stream clim-stream-pane) continuation input-sensitizer initial-contents class)
  (declare (ignore continuation input-sensitizer initial-contents class))
  (with-coalesced-flushes ()
    (call-next-method)))

;;; Queue reads and listens present the scope's pending drawing first.
;;; QUEUE-READ presents on every call, including the read of an input
;;; buffer that STREAM-READ-GESTURE makes once STREAM-INPUT-WAIT has found a
;;; gesture, which does not block.  The two timed operations present unless
;;; the timeout is 0.  QUEUE-LISTEN polls through QUEUE-LISTEN-OR-WAIT with
;;; a NIL timeout, so a plain poll presents as well.  Outside a scope none
;;; of them acts.

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
