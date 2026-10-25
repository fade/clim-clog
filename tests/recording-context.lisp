;;;; tests/recording-context.lisp
;;;;
;;;; A stand-in canvas context that records the scripts CLOG would send to a
;;;; browser, and readers for those scripts.
;;;;
;;;; The context is a subclass of CLOG:CLOG-CONTEXT2D, so CLOG's own script
;;;; builders run against it unchanged.  Its methods, and those of its text
;;;; metrics, specialize only the classes defined here: no object this file
;;;; creates reaches a connection, and nothing else in the image changes
;;;; behaviour.

(defpackage #:clim-clog/tests/recording-context
  (:use #:clim-lisp)
  (:export #:recording-context
           #:make-recording-context
           #:recorded-scripts
           #:recorded-text-metrics
           #:recorded-script
           #:js-string-argument
           #:replay-canvas
           #:paint-kind
           #:paint-width
           #:paint-subpaths
           #:subpath-elements
           #:subpath-closed-p))

(in-package #:clim-clog/tests/recording-context)

;;; ------------------------------------------------------------------
;;; Reading a single-quoted JavaScript string literal.

(defun %hex-code (script start count)
  "The integer the COUNT hexadecimal digits at START in SCRIPT denote, or NIL
when SCRIPT ends first or any of them is not a hexadecimal digit."
  (let ((end (+ start count)))
    (when (and (<= end (length script))
               (every (lambda (c) (digit-char-p c 16)) (subseq script start end)))
      (parse-integer script :start start :end end :radix 16))))

(defun %decode-js-escape (script position)
  "Decode the escape whose letter is at POSITION in SCRIPT, just after a
backslash.  Return the character and the position after the escape.  Any
escaped character other than x, u, n, r, and t stands for itself.  Answer
NIL when a browser refuses the escape:
  - SCRIPT ends;
  - a raw newline or carriage return follows;
  - \\x or \\u lacks its hexadecimal digits."
  (when (< position (length script))
    (let ((c (char script position)))
      (case c
        ((#\Newline #\Return) nil)
        (#\x (let ((code (%hex-code script (1+ position) 2)))
               (and code (values (code-char code) (+ position 3)))))
        (#\u (let ((code (%hex-code script (1+ position) 4)))
               (and code (values (code-char code) (+ position 5)))))
        (#\n (values (code-char 10) (1+ position)))
        (#\r (values (code-char 13) (1+ position)))
        (#\t (values (code-char 9) (1+ position)))
        (t (values c (1+ position)))))))

(defun %read-js-literal (script position)
  "Decode the single-quoted literal whose body starts at POSITION in SCRIPT,
just after its opening quote, and return the decoded string.  Answer NIL when
the literal holds:
  - a raw newline or carriage return;
  - an escape a browser refuses;
  - no closing quote;
  - anything but a closing parenthesis after its closing quote."
  (let ((out (make-string-output-stream))
        (end (length script))
        (i position))
    (loop
      (when (>= i end)
        (return nil))
      (let ((c (char script i)))
        (case c
          (#\' (return (and (< (1+ i) end)
                            (char= (char script (1+ i)) #\))
                            (get-output-stream-string out))))
          ((#\Newline #\Return) (return nil))
          (#\\ (multiple-value-bind (decoded next)
                   (%decode-js-escape script (1+ i))
                 (unless decoded
                   (return nil))
                 (write-char decoded out)
                 (setf i next)))
          (t (write-char c out)
             (incf i)))))))

(defun js-string-argument (script function-name)
  "The decoded string of the one single-quoted literal that is the argument of
FUNCTION-NAME's first call in SCRIPT.  Decodes \\xHH, \\uHHHH, \\\\, \\',
\\\", \\n, \\r, and \\t.  Answer NIL when SCRIPT holds no such call, or when a
browser parser refuses the literal:
  - a raw newline or carriage return inside it;
  - a backslash with nothing after it;
  - no closing quote;
  - anything but a closing parenthesis right after the closing quote."
  (let* ((call (concatenate 'string function-name "('"))
         (start (search call script)))
    (when start
      (%read-js-literal script (+ start (length call))))))

;;; ------------------------------------------------------------------
;;; The recording context.

(defclass recording-context (clog:clog-context2d)
  ((scripts :initform '() :accessor %scripts
            :documentation "The scripts sent to the context, newest first."))
  (:documentation "A canvas context that records each script CLOG sends to it
in place of a browser connection."))

(defun make-recording-context ()
  "A fresh recording context with no connection and the html-id \"recorded\"."
  (make-instance 'recording-context :connection-id nil :html-id "recorded"))

(defun recorded-scripts (context)
  "The scripts sent to CONTEXT, oldest first."
  (reverse (%scripts context)))

(defmethod clog:js-execute ((obj recording-context) script)
  "Record SCRIPT and return OBJ, as CLOG's own method returns its object."
  (push script (%scripts obj))
  obj)

(defmethod clog:js-query ((obj recording-context) script &key default-answer)
  "Record SCRIPT and answer DEFAULT-ANSWER, without reaching a connection."
  (push script (%scripts obj))
  default-answer)

;;; ------------------------------------------------------------------
;;; Measured text.

(defclass recorded-text-metrics (clog:clog-text-metrics)
  ((script :initarg :script :reader recorded-script
           :documentation "The measureText script that created the metrics."))
  (:documentation "Text metrics standing for the result of the measureText
script that created them."))

(defmethod clog:measure-text ((obj recording-context) text)
  "Send CLOG's own measureText script for TEXT through OBJ, then return a
RECORDED-TEXT-METRICS holding that script in place of CLOG's metrics."
  (let ((metrics (call-next-method)))
    (make-instance 'recorded-text-metrics
                   :connection-id nil
                   :html-id (clog:html-id metrics)
                   :script (first (%scripts obj)))))

(defmethod clog:js-query ((obj recorded-text-metrics) script &key default-answer)
  "Answer \"7.5\" when JS-STRING-ARGUMENT reads the measureText literal of the
script that created OBJ, and DEFAULT-ANSWER otherwise, as CLOG answers once
the browser has thrown."
  (declare (ignore script))
  (if (js-string-argument (recorded-script obj) "measureText")
      "7.5"
      default-answer))

;;; ------------------------------------------------------------------
;;; Replaying recorded canvas calls as geometry.

(defstruct paint
  "One stroke() or fill() of a replay, with the slots:
  - KIND, :STROKE or :FILL;
  - WIDTH, the effective stroke width in device pixels, NIL for a fill;
  - SUBPATHS, the path painted."
  kind
  width
  subpaths)

(defstruct subpath
  "One subpath of a replayed path.  ELEMENTS are in drawing order, each
(:MOVE point), (:LINE point), or (:ARC points), every point an (X . Y) cons in
device coordinates.  An arc lists 33 samples from its start to its end; the
line a canvas adds from the current point to an arc's start is implied, not
listed.  CLOSED-P is true once closePath() closes the subpath."
  (elements '())
  (closed-p nil))

(defparameter *skipped-canvas-calls*
  '("strokeStyle" "fillStyle" "font" "textAlign" "textBaseline" "globalAlpha"
    "setLineDash" "rect" "clip" "fillRect" "strokeRect" "fillText" "strokeText")
  "Canvas properties and calls REPLAY-CANVAS skips: state and drawing these
suites do not read.")

(defun %js-number-text-p (text)
  "True when TEXT is a decimal JavaScript number literal: an optional sign,
digits with at most one decimal point, and an optional exponent."
  (let ((i 0)
        (n (length text))
        (digits 0))
    (flet ((next-is (test)
             (and (< i n) (funcall test (char text i))))
           (sign-p (c)
             (member c '(#\+ #\-))))
      (when (next-is #'sign-p)
        (incf i))
      (loop while (next-is #'digit-char-p)
            do (incf i) (incf digits))
      (when (next-is (lambda (c) (char= c #\.)))
        (incf i)
        (loop while (next-is #'digit-char-p)
              do (incf i) (incf digits)))
      (when (and (plusp digits) (next-is (lambda (c) (char-equal c #\e))))
        (incf i)
        (when (next-is #'sign-p)
          (incf i))
        (unless (next-is #'digit-char-p)
          (return-from %js-number-text-p nil))
        (loop while (next-is #'digit-char-p)
              do (incf i)))
      (and (plusp digits) (= i n)))))

(defun %js-argument (text)
  "The value of the JavaScript argument TEXT: a double-float for a number
literal, :TRUE for true, and :FALSE for false.  Signals an error for anything
else."
  (cond ((string= text "true") :true)
        ((string= text "false") :false)
        ((%js-number-text-p text)
         (let ((*read-default-float-format* 'double-float)
               (*read-eval* nil))
           (float (read-from-string text) 1d0)))
        (t (error "replay-canvas: ~s is not a JavaScript number, true, or false."
                  text))))

(defun %split-arguments (text)
  "The comma-separated argument texts of TEXT; NIL when TEXT is empty."
  (unless (string= text "")
    (loop for start = 0 then (1+ comma)
          for comma = (position #\, text :start start)
          collect (subseq text start comma)
          while comma)))

(defun %canvas-call (script)
  "Split SCRIPT, as CLOG sends it to a context, into the name after its
clog['...']. prefix, :SET or :CALL, and the assigned value text or the
argument text.  Signals an error when SCRIPT lacks the prefix or is neither an
assignment nor a call."
  (let* ((prefix-end (and (eql 0 (search "clog['" script))
                          (search "']." script)))
         (body (if prefix-end
                   (subseq script (+ prefix-end 3))
                   (error "replay-canvas: ~s has no clog['...']. prefix." script)))
         (name-end (or (position-if-not (lambda (c) (or (alphanumericp c) (char= c #\_)))
                                        body)
                       (length body)))
         (name (subseq body 0 name-end))
         (next (and (< name-end (length body)) (char body name-end))))
    (cond ((eql next #\=)
           (values name :set (subseq body (1+ name-end))))
          ((and (eql next #\() (char= (char body (1- (length body))) #\)))
           (values name :call (subseq body (1+ name-end) (1- (length body)))))
          (t (error "replay-canvas: cannot read the canvas call ~s." script)))))

(defun %compose (m n)
  "The canvas matrix M times N, each a list (A B C D E F): the map that
applies N, then M."
  (destructuring-bind (a b c d e f) m
    (destructuring-bind (a2 b2 c2 d2 e2 f2) n
      (list (+ (* a a2) (* c b2)) (+ (* b a2) (* d b2))
            (+ (* a c2) (* c d2)) (+ (* b c2) (* d d2))
            (+ (* a e2) (* c f2) e) (+ (* b e2) (* d f2) f)))))

(defun %map-point (m x y)
  "The point (X, Y) mapped through the canvas matrix M, as an (X . Y) cons."
  (destructuring-bind (a b c d e f) m
    (cons (+ (* a x) (* c y) e) (+ (* b x) (* d y) f))))

(defun %arc-points (matrix cx cy rx ry rotation start end anticlockwise)
  "Sample 33 points along an elliptical arc as a canvas draws it, mapped
through MATRIX.  The arc sweeps from START to END in increasing angle
(clockwise on the canvas), or in decreasing angle when ANTICLOCKWISE is true.
A sweep of 2 pi or more in the drawing direction is the whole ellipse; any
other difference is reduced modulo 2 pi in that direction."
  (let* ((two-pi (* 2 pi))
         (sweep (if anticlockwise
                    (if (>= (- start end) two-pi)
                        (- two-pi)
                        (- (mod (- start end) two-pi)))
                    (if (>= (- end start) two-pi)
                        two-pi
                        (mod (- end start) two-pi)))))
    (loop for i from 0 to 32
          collect (let* ((angle (+ start (* sweep (/ i 32))))
                         (x (* rx (cos angle)))
                         (y (* ry (sin angle))))
                    (%map-point matrix
                                (+ cx (- (* x (cos rotation)) (* y (sin rotation))))
                                (+ cy (* x (sin rotation)) (* y (cos rotation))))))))

(defun %check-arguments (name args numbers &optional flag)
  "Return ARGS of the canvas call NAME when they are NUMBERS numbers followed,
when FLAG is true, by an optional :TRUE or :FALSE.  Signals an error naming
the call otherwise."
  (let ((count (length args)))
    (unless (and (or (= count numbers) (and flag (= count (1+ numbers))))
                 (every #'realp (subseq args 0 (min count numbers)))
                 (or (= count numbers) (member (car (last args)) '(:true :false))))
      (error "replay-canvas: ~a takes ~d number~:p~:[~;, then optionally true or false~], not ~s."
             name numbers flag args))
    args))

(defun replay-canvas (scripts)
  "Read SCRIPTS in order as a canvas would, after each one's clog['...']. prefix,
and return the paints, one per stroke() or fill() in order.  Give a stroke the
width lineWidth times the square root of the absolute determinant of the
transform current at the stroke.  Map each point to device coordinates through
the transform current when it was added.  Interpret these calls:
  - save() and restore(), a stack of transform and lineWidth;
  - transform(), the current matrix times the given one, and resetTransform();
  - lineWidth=;
  - beginPath(), moveTo(), lineTo(), and closePath();
  - arc(x,y,r,start,end[,anticlockwise]);
  - ellipse(x,y,rx,ry,rotation,start,end[,anticlockwise]);
  - stroke() and fill().
Start a new subpath for lineTo() or arc() when none is open, as after
closePath().  Skip the calls in *SKIPPED-CANVAS-CALLS*.  Signal an error
naming any other call, and for any argument that is not a JavaScript number,
true, or false."
  (let ((identity (list 1d0 0d0 0d0 1d0 0d0 0d0)))
    (let ((matrix identity)
          (line-width 1d0)
          (stack '())
          (subpaths '())
          (current nil)
          (paints '()))
      (flet ((add (element)
               (if current
                   (setf (subpath-elements current)
                         (append (subpath-elements current) (list element)))
                   (push (setf current (make-subpath :elements (list element)))
                         subpaths)))
             (painted ()
               (mapcar (lambda (s)
                         (make-subpath :elements (copy-list (subpath-elements s))
                                       :closed-p (subpath-closed-p s)))
                       (reverse subpaths))))
        (dolist (script scripts (nreverse paints))
          (multiple-value-bind (name kind text) (%canvas-call script)
            (cond
              ((member name *skipped-canvas-calls* :test #'string=))
              ((eq kind :set)
               (let ((value (and (string= name "lineWidth") (%js-argument text))))
                 (unless (realp value)
                   (error "replay-canvas: cannot replay ~s." script))
                 (setf line-width value)))
              (t
               (let ((args (mapcar #'%js-argument (%split-arguments text))))
                 (flet ((check (numbers &optional flag)
                          (%check-arguments name args numbers flag)))
                   (cond
                     ((string= name "save")
                      (check 0)
                      (push (list matrix line-width) stack))
                     ((string= name "restore")
                      (check 0)
                      (when stack
                        (destructuring-bind (m w) (pop stack)
                          (setf matrix m
                                line-width w))))
                     ((string= name "transform")
                      (setf matrix (%compose matrix (check 6))))
                     ((string= name "resetTransform")
                      (check 0)
                      (setf matrix identity))
                     ((string= name "beginPath")
                      (check 0)
                      (setf subpaths '()
                            current nil))
                     ((string= name "moveTo")
                      (destructuring-bind (x y) (check 2)
                        (setf current nil)
                        (add (list :move (%map-point matrix x y)))))
                     ((string= name "lineTo")
                      (destructuring-bind (x y) (check 2)
                        (add (list (if current :line :move) (%map-point matrix x y)))))
                     ((string= name "closePath")
                      (check 0)
                      (when current
                        (setf (subpath-closed-p current) t
                              current nil)))
                     ((string= name "arc")
                      (destructuring-bind (x y r start end &optional flag) (check 5 t)
                        (add (list :arc (%arc-points matrix x y r r 0 start end
                                                     (eq flag :true))))))
                     ((string= name "ellipse")
                      (destructuring-bind (x y rx ry rotation start end &optional flag)
                          (check 7 t)
                        (add (list :arc (%arc-points matrix x y rx ry rotation start end
                                                     (eq flag :true))))))
                     ((string= name "stroke")
                      (check 0)
                      (destructuring-bind (a b c d e f) matrix
                        (declare (ignore e f))
                        (push (make-paint :kind :stroke
                                          :width (* line-width (sqrt (abs (- (* a d) (* b c)))))
                                          :subpaths (painted))
                              paints)))
                     ((string= name "fill")
                      (check 0)
                      (push (make-paint :kind :fill :subpaths (painted)) paints))
                     (t (error "replay-canvas: unknown canvas call ~a in ~s."
                               name script)))))))))))))
