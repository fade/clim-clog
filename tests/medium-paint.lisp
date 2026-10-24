;;;; tests/medium-paint.lisp
;;;;
;;;; Headless contract tests for how the clim-clog medium paints: the CSS
;;;; colours its default inks resolve to.
;;;;
;;;; These need no browser.  They run on a bare CLIM:BASIC-MEDIUM (the
;;;; CLOG-MEDIUM superclass), constructed with explicit foreground and
;;;; background colours and no port, graft, or connection.

(defpackage #:clim-clog/tests/medium-paint
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:clim-clog/tests/medium-paint)

(defun %medium (&rest initargs)
  "A bare basic-medium built from INITARGS: no port, no canvas."
  (apply #'make-instance 'basic-medium initargs))

(defun %coloured-medium ()
  "A bare medium whose foreground and background are distinct solid colours."
  (%medium :foreground (make-rgb-color 0.2 0.4 0.6)
           :background (make-rgb-color 0.9 0.8 0.7)))

;;; ------------------------------------------------------------------
;;; The default inks resolve through the medium's colours.
;;; ------------------------------------------------------------------

(define-test foreground-ink-resolves-through-the-medium
  (let ((css (clim-clog::ink->css +foreground-ink+ (%coloured-medium))))
    (is equal "rgb(51,102,153)" css)
    (false (equal "black" css))))

(define-test background-ink-resolves-through-the-medium
  ;; 0.7 * 255 is 178.5, which ROUND takes to the even 178.
  (let ((css (clim-clog::ink->css +background-ink+ (%coloured-medium))))
    (is equal "rgb(230,204,178)" css)
    (false (equal "white" css))))

(define-test an-indirect-medium-colour-resolves-one-level
  ;; The medium's foreground is itself an indirect ink: it renders as it
  ;; would without a medium, rather than resolving again through the medium.
  (is equal "white"
      (clim-clog::ink->css +foreground-ink+ (%medium :foreground +background-ink+))))

;;; ------------------------------------------------------------------
;;; Fixed renderings: the default inks without a medium, and the
;;; transparent inks.
;;; ------------------------------------------------------------------

(define-test indirect-inks-keep-their-literals-without-a-medium
  (is equal "black" (clim-clog::ink->css +foreground-ink+))
  (is equal "white" (clim-clog::ink->css +background-ink+)))

(define-test flipping-and-transparent-inks-stay-transparent
  (let ((m (%coloured-medium)))
    (is equal "rgba(0,0,0,0)" (clim-clog::ink->css +flipping-ink+))
    (is equal "rgba(0,0,0,0)" (clim-clog::ink->css +transparent-ink+))
    (is equal "rgba(0,0,0,0)" (clim-clog::ink->css +flipping-ink+ m))
    (is equal "rgba(0,0,0,0)" (clim-clog::ink->css +transparent-ink+ m))))
