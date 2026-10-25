;;;; tests/ellipse-arcs.lisp
;;;;
;;;; Headless contract tests for the canvas calls that drawing an elliptical
;;;; arc makes:
;;;;   - where each arc lies;
;;;;   - how wide its stroke reads;
;;;;   - how a filled arc is closed.
;;;;
;;;; These need no browser.  Each draws on a bare CLIM-CLOG:CLOG-MEDIUM whose
;;;; drawable is a fresh recording context, then reads the recorded scripts
;;;; back as geometry through REPLAY-CANVAS.

(defpackage #:clim-clog/tests/ellipse-arcs
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false)
  (:import-from #:clim-clog/tests/recording-context
                #:make-recording-context
                #:recorded-scripts
                #:replay-canvas
                #:paint-kind
                #:paint-width
                #:paint-subpaths
                #:subpath-elements
                #:subpath-closed-p))

(in-package #:clim-clog/tests/ellipse-arcs)

(defparameter *corner-squares*
  '((100 108 100 108) (172 180 100 108) (100 108 116 124) (172 180 116 124))
  "The corner squares (X1 X2 Y1 Y2) of the rectangle (100,100)-(180,124) with
radius 8, in reading order, top left first.")

(defparameter *corner-centres*
  '((108 . 108) (172 . 108) (108 . 116) (172 . 116))
  "The corner centres of the rectangle (100,100)-(180,124) with radius 8.")

(defun %replayed-drawing (draw)
  "Call DRAW with a bare clog-medium and return the paints REPLAY-CANVAS reads
from its fresh recording context."
  (let ((ctx (make-recording-context))
        (medium (make-instance 'clim-clog:clog-medium)))
    (setf (medium-drawable medium) ctx)
    (funcall draw medium)
    (replay-canvas (recorded-scripts ctx))))

(defun %rounded-rectangle (filled)
  "The paints of the rectangle (100,100)-(180,124) drawn with corners of
radius 8, filled when FILLED is true."
  (%replayed-drawing
   (lambda (medium)
     (clime:draw-rounded-rectangle* medium 100 100 180 124
                                    :radius 8 :filled filled))))

(defun %arcs (paints)
  "The sample lists of every arc in PAINTS, in order."
  (loop for paint in paints
        append (loop for subpath in (paint-subpaths paint)
                     append (loop for element in (subpath-elements subpath)
                                  when (eq (first element) :arc)
                                    collect (second element)))))

(defun %arc-ends (arcs)
  "The first and last sample of each arc in ARCS."
  (mapcar (lambda (arc) (list (first arc) (first (last arc)))) arcs))

(defun %near-p (point x y)
  "True when POINT, an (X . Y) cons, lies within 0.01 of (X, Y)."
  (and point
       (<= (sqrt (+ (expt (- (car point) x) 2) (expt (- (cdr point) y) 2)))
           0.01)))

(defun %in-square-p (point square)
  "True when POINT lies inside SQUARE, (X1 X2 Y1 Y2), within 0.01."
  (destructuring-bind (x1 x2 y1 y2) square
    (and (<= (- x1 0.01) (car point) (+ x2 0.01))
         (<= (- y1 0.01) (cdr point) (+ y2 0.01)))))

(defun %arc-in-square-p (arc square)
  "True when every sample of ARC lies inside SQUARE."
  (every (lambda (point) (%in-square-p point square)) arc))

(defun %distance-to-segment (a b x y)
  "The least distance from (X, Y) to the segment from point A to point B."
  (let* ((dx (- (car b) (car a)))
         (dy (- (cdr b) (cdr a)))
         (length-squared (+ (* dx dx) (* dy dy)))
         (s (if (zerop length-squared)
                0
                (max 0 (min 1 (/ (+ (* (- x (car a)) dx) (* (- y (cdr a)) dy))
                                 length-squared))))))
    (sqrt (+ (expt (- x (+ (car a) (* s dx))) 2)
             (expt (- y (+ (cdr a) (* s dy))) 2)))))

(defun %distance-to-polyline (points x y)
  "The least distance from (X, Y) to the polyline through POINTS."
  (loop for (a b) on points
        while b
        minimize (%distance-to-segment a b x y)))

(defun %scripts (&rest calls)
  "CALLS as a recording context records them, each with the clog['recorded'].
prefix."
  (mapcar (lambda (call) (concatenate 'string "clog['recorded']." call)) calls))

(defun %signals-error-p (thunk)
  "True when calling THUNK signals an error."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

;;; ------------------------------------------------------------------
;;; Rounded rectangles: each corner arc inside its own corner.

(define-test rounded-outline-strokes-one-device-pixel
  (let* ((strokes (remove-if-not (lambda (paint) (eq (paint-kind paint) :stroke))
                                 (%rounded-rectangle nil)))
         (widths (mapcar #'paint-width strokes)))
    (true strokes "The outline is stroked.")
    (true (every (lambda (width) (< (abs (- width 1)) 0.001)) widths)
          "Every stroke is one device pixel wide: ~s" widths)))

(define-test rounded-corners-arc-through-their-own-corners
  (dolist (filled '(nil t))
    (let ((arcs (%arcs (%rounded-rectangle filled))))
      (is = 4 (length arcs)
          "~:[Outlined~;Filled~] corners make four arcs." filled)
      (true (every (lambda (arc)
                     (some (lambda (square) (%arc-in-square-p arc square))
                           *corner-squares*))
                   arcs)
            "~:[Outlined~;Filled~], every arc lies inside a corner square; arc ends: ~s"
            filled (%arc-ends arcs))
      (true (every (lambda (square)
                     (= 1 (count-if (lambda (arc) (%arc-in-square-p arc square))
                                    arcs)))
                   *corner-squares*)
            "~:[Outlined~;Filled~], each corner square holds one arc; arc ends: ~s"
            filled (%arc-ends arcs)))))

(define-test filled-corners-are-pies
  (let ((subpaths (loop for paint in (%rounded-rectangle t)
                        when (eq (paint-kind paint) :fill)
                          append (remove-if-not
                                  (lambda (subpath)
                                    (find :arc (subpath-elements subpath) :key #'first))
                                  (paint-subpaths paint)))))
    (is = 4 (length subpaths) "Each filled corner is painted as one subpath holding its arc.")
    (dolist (subpath subpaths)
      (let ((kinds (mapcar #'first (subpath-elements subpath))))
        (true (and (eq :move (first kinds)) (eq :arc (second kinds)))
              "The subpath opens with a move immediately before the arc: ~s" kinds)
        (true (subpath-closed-p subpath) "The pie's subpath is closed.")))
    (true (every (lambda (centre)
                   (some (lambda (subpath)
                           (let ((head (first (subpath-elements subpath))))
                             (and (eq :move (first head))
                                  (%near-p (second head) (car centre) (cdr centre)))))
                         subpaths))
                 *corner-centres*)
          "Each corner's pie starts at its corner centre; subpaths open with ~s"
          (mapcar (lambda (subpath)
                    (let ((head (first (subpath-elements subpath))))
                      (if (eq :move (first head)) head (first head))))
                  subpaths))))

;;; ------------------------------------------------------------------
;;; Single ellipses.

(define-test an-elliptical-arc-ends-on-its-rays
  (dolist (second-radius '((0 10) (0 -10)))
    (destructuring-bind (r2x r2y) second-radius
      (let* ((arcs (%arcs (%replayed-drawing
                           (lambda (medium)
                             (medium-draw-ellipse* medium 50 50 20 0 r2x r2y
                                                   (/ pi 4) (/ pi 2) nil)))))
             (arc (first arcs)))
        (is = 1 (length arcs) "One arc with the second radius ~s." second-radius)
        (true (%near-p (first arc) 58.944 41.056)
              "With the second radius ~s the arc starts on the 45-degree ray at (58.944, 41.056), not ~s."
              second-radius (first arc))
        (true (%near-p (first (last arc)) 50 40)
              "With the second radius ~s the arc ends on the 90-degree ray at (50, 40), not ~s."
              second-radius (first (last arc)))
        (true (and arc
                   (every (lambda (point)
                            (and (>= (car point) 49.99) (<= (cdr point) 50.01)))
                          arc))
              "With the second radius ~s every sample lies up and to the right of the centre."
              second-radius)))))

(define-test a-flat-ellipse-draws-nothing
  (let ((paints (%replayed-drawing
                 (lambda (medium)
                   (medium-draw-ellipse* medium 50 50 8 0 16 0 0 (* 2 pi) nil)))))
    (is = 0 (length paints)
        "Radius vectors on one line leave no stroke and no fill, not ~s."
        (mapcar #'paint-kind paints))))

(define-test a-full-circle-is-whole
  (let* ((paints (%replayed-drawing
                  (lambda (medium)
                    (medium-draw-ellipse* medium 50 50 8 0 0 8 0 (* 2 pi) t))))
         (subpaths (and paints (paint-subpaths (first paints))))
         (samples (first (%arcs paints))))
    (is equal '(:fill) (mapcar #'paint-kind paints) "A full circle is one fill.")
    (is equal '((:arc)) (mapcar (lambda (subpath) (mapcar #'first (subpath-elements subpath)))
                                subpaths)
        "Its subpath is one arc and nothing else.")
    (true (and samples
               (every (lambda (point)
                        (< (abs (- (sqrt (+ (expt (- (car point) 50) 2)
                                            (expt (- (cdr point) 50) 2)))
                                   8))
                           0.01))
                      samples))
          "Every sample lies 8 device pixels from the centre.")
    (true (every (lambda (quarter)
                   (some (lambda (point)
                           (and (> (* (car quarter) (- (car point) 50)) 0.01)
                                (> (* (cdr quarter) (- (cdr point) 50)) 0.01)))
                         samples))
                 '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))
          "The samples reach all four quarters around the centre.")))

(define-test a-skewed-ellipse-keeps-its-shape
  (let ((samples (loop for arc in (%arcs (%replayed-drawing
                                          (lambda (medium)
                                            (medium-draw-ellipse* medium 60 60 10 0 4 6
                                                                  0 (* 2 pi) t))))
                       append arc)))
    (true samples "The ellipse is drawn.")
    (true (every (lambda (point)
                   (let* ((qx (- (car point) 60))
                          (qy (- (cdr point) 60))
                          (ux (/ (- (* 6 qx) (* 4 qy)) 60))
                          (uy (/ (* 10 qy) 60)))
                     (< (abs (- (sqrt (+ (* ux ux) (* uy uy))) 1)) 0.001)))
                 samples)
          "Every sample maps back onto the unit circle through the inverse radius matrix.")))

;;; ------------------------------------------------------------------
;;; Control: the replay reads the defects these tests check for.

(define-test the-replay-reads-a-scaled-stroke
  (let* ((scaled (replay-canvas
                  (%scripts "save()" "transform(8,0,0,8,100,100)" "beginPath()"
                            "arc(0,0,1,0,1.5707964)" "lineWidth=1" "stroke()" "restore()")))
         (unscaled (replay-canvas
                    (%scripts "save()" "beginPath()"
                              "arc(0,0,1,0,1.5707964)" "lineWidth=1" "stroke()" "restore()")))
         (anticlockwise (replay-canvas
                         (%scripts "save()" "beginPath()"
                                   "arc(0,0,1,0,1.5707964,true)" "lineWidth=1" "stroke()"
                                   "restore()")))
         (scaled-arc (first (%arcs scaled)))
         (clockwise-arc (first (%arcs unscaled)))
         (anticlockwise-arc (first (%arcs anticlockwise))))
    (is = 1 (length scaled) "The scaled script strokes once.")
    (true (< (abs (- (paint-width (first scaled)) 8)) 0.001)
          "A stroke under a scale of 8 reads 8 device pixels wide, not ~s." (paint-width (first scaled)))
    (true (%near-p (first (last scaled-arc)) 100 108)
          "The scaled arc ends at (100, 108), not ~s." (first (last scaled-arc)))
    (is = 1 (length unscaled) "The unscaled script strokes once.")
    (true (< (abs (- (paint-width (first unscaled)) 1)) 0.001)
          "An unscaled stroke reads 1 device pixel wide, not ~s." (paint-width (first unscaled)))
    (true (%near-p (first (last clockwise-arc)) 0 1)
          "The unscaled arc ends at (0, 1), not ~s." (first (last clockwise-arc)))
    (true (<= (%distance-to-polyline anticlockwise-arc 0 -1) 0.01)
          "With anticlockwise true, the sweep passes (0, -1).")
    (false (<= (%distance-to-polyline clockwise-arc 0 -1) 0.01)
           "Without it the sweep does not pass (0, -1).")
    (true (%signals-error-p
           (lambda () (replay-canvas (%scripts "setYransform(1,0,0,1,0,0,0)"))))
          "A misspelt canvas call signals an error.")
    (true (%signals-error-p
           (lambda () (replay-canvas (%scripts "arc(0,0,1,0,1.5707964,T)"))))
          "An argument that is not a number, true, or false signals an error.")))
