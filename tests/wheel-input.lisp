;;;; tests/wheel-input.lisp
;;;;
;;;; Headless contract tests for how clim-clog turns a browser wheel event
;;;; into the initargs of a CLIM POINTER-SCROLL-EVENT.
;;;;
;;;; These need no browser.  They feed WHEEL-SCROLL-INITARGS the plist CLOG
;;;; delivers for a wheel event (:DELTA-X, :DELTA-Y, :DELTA-Z, and
;;;; :DELTA-MODE) and check the pixel deltas and wheel button it returns.
;;;; Deltas compare under EQUALP, which compares numbers with =, so 120.0
;;;; matches 120 and a missing delta (NIL) fails the assertion rather than
;;;; signalling a type error.

(defpackage #:clim-clog/tests/wheel-input
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

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
