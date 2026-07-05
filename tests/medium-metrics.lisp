;;;; tests/medium-metrics.lisp
;;;;
;;;; Headless contract tests for the clim-clog medium text-metric protocol.
;;;;
;;;; These need no browser: they exercise the offline/fallback metric path,
;;;; which is deliberately font-size-relative and requires no <canvas>.  They
;;;; pin the *arity and type* contract that mcclim's text layout consumes --
;;;; the class of bug that blanked the frame when TEXT-BOUNDING-RECTANGLE*
;;;; returned four values while mcclim-20260101's TEXT-METRICS unpacked six
;;;; (feeding a NIL cursor advance into STREAM-CURSOR-MOTION -> (+ NIL 0)).
;;;;
;;;; We test on a bare CLIM:BASIC-MEDIUM (the CLOG-MEDIUM superclass) because
;;;; that is exactly the mediumless sheet mcclim measures during layout, and
;;;; it is constructible without a port/graft/connection.  A realised
;;;; CLOG-MEDIUM's browser-measured path is covered by the runnable
;;;; differential harness in examples/hello.lisp.

(defpackage #:clim-clog/tests/medium-metrics
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true))

(in-package #:clim-clog/tests/medium-metrics)

(defun %medium ()
  "A bare basic-medium: no port, no canvas -- the ungrafted-layout case."
  (make-instance 'basic-medium))

(defun %ts (&optional (size 12))
  (make-text-style :sans-serif :roman size))

;;; ------------------------------------------------------------------
;;; TEXT-SIZE: five values (width height cursor-dx cursor-dy baseline).
;;; ------------------------------------------------------------------

(define-test text-size-returns-five-reals
  (let ((vals (multiple-value-list (text-size (%medium) "hello"))))
    (is = 5 (length vals))
    (true (every #'realp vals))
    (destructuring-bind (w h cx cy base) vals
      (declare (ignore cx cy base))
      (true (plusp w))
      (true (plusp h)))))

(define-test text-size-accepts-a-character
  (multiple-value-bind (w h) (text-size (%medium) #\M)
    (true (realp w))
    (true (realp h))
    (true (plusp w))))

(define-test text-size-multiline-is-taller
  (let ((one (nth-value 1 (text-size (%medium) "a")))
        (two (nth-value 1 (text-size (%medium) (format nil "a~%b")))))
    (true (> two one))))

(define-test text-size-widens-with-length
  (let ((short (nth-value 0 (text-size (%medium) "i")))
        (long  (nth-value 0 (text-size (%medium) "iiiiiiiiii"))))
    (true (> long short))))

(define-test text-size-tolerates-nil-text-style
  (true (realp (nth-value 0 (text-size (%medium) "x" :text-style nil)))))

;;; ------------------------------------------------------------------
;;; TEXT-BOUNDING-RECTANGLE*: SIX values (xmin ymin xmax ymax dx dy).
;;; This is the exact contract that regressed.
;;; ------------------------------------------------------------------

(define-test text-bounding-rectangle-returns-six-reals
  (let ((vals (multiple-value-list (text-bounding-rectangle* (%medium) "hello"))))
    (is = 6 (length vals))
    (true (every #'realp vals))))

(define-test text-bounding-rectangle-advance-is-positive
  ;; The fifth value (dx, the cursor advance) is what mcclim feeds into
  ;; STREAM-CURSOR-MOTION; it must be a positive real, never NIL.
  (multiple-value-bind (xmin ymin xmax ymax dx dy)
      (text-bounding-rectangle* (%medium) "hello")
    (declare (ignore xmin ymin xmax ymax dy))
    (true (realp dx))
    (true (plusp dx))))

;;; ------------------------------------------------------------------
;;; The real mcclim -> backend path: TEXT-METRICS (string) unpacks six
;;; values from TEXT-BOUNDING-RECTANGLE*.  Driving the actual mcclim
;;; function catches contract drift on either side of the seam.
;;; ------------------------------------------------------------------

(define-test text-metrics-string-contract-holds
  (let ((vals (multiple-value-list
               (clim-internals::text-metrics (%medium) "hello" :text-style (%ts)))))
    (is = 6 (length vals))
    (true (every #'realp vals))))

;;; ------------------------------------------------------------------
;;; Per-text-style metrics are all real and positive.
;;; ------------------------------------------------------------------

(define-test text-style-metrics-are-positive-reals
  (let ((m (%medium)) (ts (%ts)))
    (true (plusp (text-style-ascent ts m)))
    (true (plusp (text-style-descent ts m)))
    (true (plusp (text-style-height ts m)))
    (true (plusp (text-style-width ts m)))
    (true (plusp (text-style-character-width ts m #\x)))))

(define-test text-style-height-scales-with-size
  (let ((m (%medium)))
    (true (> (text-style-height (%ts 24) m)
             (text-style-height (%ts 12) m)))))
