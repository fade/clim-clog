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
           #:js-string-argument))

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
