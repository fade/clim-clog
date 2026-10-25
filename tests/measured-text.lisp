;;;; tests/measured-text.lisp
;;;;
;;;; Headless contract tests for the text clim-clog hands CLOG to measure: the
;;;; script literal it reaches the canvas in, and the width it measures.
;;;;
;;;; These need no browser.  Each runs on a fresh recording context, which
;;;; records the script CLOG's own MEASURE-TEXT builds and answers a width
;;;; only when a browser would parse that script.

(defpackage #:clim-clog/tests/measured-text
  (:use #:clim-lisp)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false)
  (:import-from #:clim-clog/tests/recording-context
                #:make-recording-context
                #:recorded-scripts
                #:js-string-argument))

(in-package #:clim-clog/tests/measured-text)

(defun %measured-script (text)
  "The last script a fresh recording context records when TEXT is measured."
  (let ((ctx (make-recording-context)))
    (clim-clog::%measure-text ctx text)
    (first (last (recorded-scripts ctx)))))

;;; ------------------------------------------------------------------
;;; Text a browser would misread reaches the canvas escaped.

(define-test a-quote-reaches-measure-text-escaped
  (let ((script (%measured-script "'")))
    (true (search "measureText('\\x27')" script)
          "The quote reaches the canvas as \\x27: ~s" script)
    (is equal "'" (js-string-argument script "measureText")
        "The literal reads back as the quote.")))

(define-test a-backslash-reaches-measure-text-escaped
  (let ((script (%measured-script (string #\\))))
    (true (search "measureText('\\x5C')" script)
          "The backslash reaches the canvas as \\x5C: ~s" script)
    (is equal (string #\\) (js-string-argument script "measureText")
        "The literal reads back as the backslash.")))

(define-test a-newline-reaches-measure-text-escaped
  (let ((script (%measured-script (string #\Newline))))
    (true (search "measureText('\\x0A')" script)
          "The newline reaches the canvas as \\x0A: ~s" script)
    (is equal (string #\Newline) (js-string-argument script "measureText")
        "The literal reads back as the newline.")))

(define-test a-quote-measures-a-width
  (let* ((ctx (make-recording-context))
         (width (clog:width (clim-clog::%measure-text ctx "'"))))
    (true (and (realp width) (> width 0))
          "A quote measures a width above 0, not ~s." width)))

;;; ------------------------------------------------------------------
;;; Guard and control.

(define-test plain-text-measures-unchanged
  (let* ((ctx (make-recording-context))
         (metrics (clim-clog::%measure-text ctx "m"))
         (script (first (last (recorded-scripts ctx)))))
    (true (search "measureText('m')" script)
          "Plain text reaches the canvas as it is: ~s" script)
    (is equal "m" (js-string-argument script "measureText"))
    (is = 7.5 (clog:width metrics))))

(define-test the-literal-reader-refuses-what-a-browser-refuses
  (false (js-string-argument "c.measureText(''')" "measureText")
         "A bare quote ends the literal early.")
  (false (js-string-argument "c.measureText('\\')" "measureText")
         "A lone backslash escapes the closing quote.")
  (false (js-string-argument (format nil "c.measureText('~c')" #\Newline)
                             "measureText")
         "A raw newline ends the line inside the literal.")
  (false (js-string-argument "c.measureText('abc" "measureText")
         "A literal with no closing quote is refused.")
  (is equal "'" (js-string-argument "c.measureText('\\x27')" "measureText")
      "An escaped quote reads back as the quote.")
  (is equal "\\x27" (js-string-argument "c.measureText('\\x5Cx27')" "measureText")
      "A doubly escaped quote reads back as the four characters of its escape."))
