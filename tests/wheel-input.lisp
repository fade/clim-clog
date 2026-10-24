;;;; tests/wheel-input.lisp
;;;;
;;;; Headless contract tests for how clim-clog turns a browser wheel event
;;;; into a CLIM POINTER-SCROLL-EVENT.
;;;;
;;;; These need no browser.  They feed WHEEL-SCROLL-INITARGS the plist CLOG
;;;; delivers for a wheel event (:DELTA-X, :DELTA-Y, :DELTA-Z, and
;;;; :DELTA-MODE) and check the pixel deltas and wheel button it returns.
;;;; Deltas compare under EQUALP, which compares numbers with =, so 120.0
;;;; matches 120 and a missing delta (NIL) fails the assertion rather than
;;;; signalling a type error.
;;;;
;;;; They also feed MAKE-CLOG-WHEEL-EVENT the same plist, which carries no
;;;; :X or :Y, on a bare port and sheet.  The event takes the position last
;;;; recorded on the port's pointer.  A still wheel makes no event, and
;;;; modifier keys in the plist reach the event's modifier state.

(defpackage #:clim-clog/tests/wheel-input
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false
                #:finish))

(in-package #:clim-clog/tests/wheel-input)

(defun %wheel (delta-x delta-y mode &optional (page-height 700))
  "Return the scroll initargs for a CLOG wheel plist with these deltas."
  (clim-clog::wheel-scroll-initargs
   (list :event-type :wheel
         :delta-x (float delta-x)
         :delta-y (float delta-y)
         :delta-z 0.0
         :delta-mode mode)
   :page-height page-height))

;;; ------------------------------------------------------------------
;;; Deltas arrive in pixels, lines, or pages and leave in pixels.
;;; ------------------------------------------------------------------

(define-test wheel-pixels-pass-through
  (let ((args (%wheel 0 120 0)))
    (is equalp 120 (getf args :delta-y))
    (is equalp 0 (getf args :delta-x))))

(define-test wheel-lines-scale-by-the-line-height
  (is = 16 clim-clog::*wheel-line-pixels*)
  (is equalp (* 3 clim-clog::*wheel-line-pixels*)
      (getf (%wheel 0 3 1) :delta-y)))

(define-test wheel-pages-scale-by-the-sheet-height
  (is equalp 700 (getf (%wheel 0 1 2 700) :delta-y)))

(define-test wheel-direction-keeps-its-sign
  (is equalp -120 (getf (%wheel 0 -120 0) :delta-y))
  (is equalp -32 (getf (%wheel 0 -2 1) :delta-y))
  (is equalp 40 (getf (%wheel 40 0 0) :delta-x)))

;;; ------------------------------------------------------------------
;;; The wheel button names the direction of travel.
;;; ------------------------------------------------------------------

(define-test wheel-button-follows-the-direction
  (is eql +pointer-wheel-down+ (getf (%wheel 0 120 0) :button))
  (is eql +pointer-wheel-up+ (getf (%wheel 0 -120 0) :button))
  (is eql +pointer-wheel-right+ (getf (%wheel 40 0 0) :button))
  (is eql +pointer-wheel-left+ (getf (%wheel -40 0 0) :button)))

(define-test a-still-wheel-makes-no-event
  (false (%wheel 0 0 0))
  (false (%wheel 0 0 1)))

;;; ------------------------------------------------------------------
;;; A wheel plist becomes a scroll event at the pointer's position.
;;;
;;; CLOG's wheel plist carries no :X or :Y.  The event takes the last
;;; position recorded on the port's pointer, on a bare BASIC-PORT holding a
;;; CLOG-POINTER and a BASIC-SHEET with a 300x700 region: no connection.
;;; ------------------------------------------------------------------

(defun %wheel-event (data &optional position)
  "Return the wheel event made from the CLOG plist DATA.
POSITION, a list (X Y), is recorded on the pointer first when given."
  (let* ((pointer (make-instance 'clim-clog::clog-pointer))
         (port (make-instance 'basic-port :pointer pointer))
         (sheet (make-instance 'basic-sheet
                               :region (make-rectangle* 0 0 300 700))))
    (when position
      (setf (pointer-position pointer) (values-list position)))
    (clim-clog::make-clog-wheel-event sheet port data)))

(define-test a-wheel-without-coordinates-takes-the-pointer-position
  (let ((data (list :delta-x 0.0 :delta-y 3.0 :delta-mode 1)))
    (finish (%wheel-event data '(37 52)))
    (let ((event (%wheel-event data '(37 52))))
      (true (typep event 'pointer-scroll-event))
      (is = 37 (pointer-event-x event))
      (is = 52 (pointer-event-y event))
      (is equalp (* 3 clim-clog::*wheel-line-pixels*)
          (pointer-event-delta-y event))
      (is eql +pointer-wheel-down+ (pointer-event-button event)))))

(define-test a-still-wheel-makes-no-scroll-event
  (false (%wheel-event (list :delta-x 0.0 :delta-y 0.0 :delta-mode 0) '(37 52))))

(define-test a-wheel-before-any-motion-has-real-coordinates
  (let ((event (%wheel-event (list :delta-x 0.0 :delta-y -120.0 :delta-mode 0))))
    (true (realp (pointer-event-x event)))
    (true (realp (pointer-event-y event)))))

(define-test wheel-modifier-keys-reach-the-event
  (flet ((modifiers (&rest keys)
           (event-modifier-state
            (%wheel-event (list* :delta-x 0.0 :delta-y 120.0 :delta-mode 0 keys)))))
    (is = 0 (modifiers))
    (is = +shift-key+ (modifiers :shift-key t))
    (is = (logior +shift-key+ +control-key+)
        (modifiers :shift-key t :ctrl-key t))
    (is = +meta-key+ (modifiers :alt-key t))))
