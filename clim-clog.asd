;;;; clim-clog.asd
;;;;
;;;; ASDF system definitions for clim-clog: a McCLIM backend that renders
;;;; to an HTML5 <canvas> in a web browser, driven over CLOG's websocket
;;;; connection.
;;;;
;;;; See README.org for goals and status, and DESIGN.org for the
;;;; architecture and the CLIM -> CLOG mapping.

(in-package #:asdf-user)

(defsystem "clim-clog"
  :description "A McCLIM backend that draws to an HTML5 canvas via CLOG."
  :author "Glenn Thompson"
  :license "BSD-3-Clause"
  :version "0.0.1"
  ;; clim-core is the portable McCLIM core (same dependency mcclim-null
  ;; uses); clog supplies the browser connection and the canvas API.
  :depends-on ("clim-core" "clog")
  :pathname "src"
  :serial nil
  :components
  ((:file "package")
   (:file "port"          :depends-on ("package"))
   (:file "graft"         :depends-on ("package" "port"))
   (:file "medium"        :depends-on ("package" "port"))
   (:file "input"         :depends-on ("package" "port"))
   (:file "coalesced-flush" :depends-on ("package" "port"))
   (:file "resize"        :depends-on ("package" "port" "graft" "input"))
   (:file "frame-manager" :depends-on ("package" "port" "medium"))))

(defsystem "clim-clog/examples"
  :description "Runnable differential-test harness for the clim-clog backend."
  :depends-on ("clim-clog" "clog")
  :pathname "examples"
  :components ((:file "hello")))

;;; Headless contract tests (Parachute), matching the valis/ubik pattern.
;;; They need no browser and run in CI.  They cover:
;;;   - the medium's default inks;
;;;   - its text metrics, via the offline fallback path;
;;;   - the factoring of a browser wheel event into scroll-event initargs;
;;;   - the text handed to CLOG to measure, recorded by a stand-in canvas
;;;     context;
;;;   - the canvas calls that drawing an elliptical arc makes, read back from
;;;     that context by a replay of its recorded scripts;
;;;   - how many frames one event presents, and the order browser input is
;;;     queued in, on a frame whose CLOG connection is a recording stand-in;
;;;   - a frame, its canvases, and the port following a browser window
;;;     resize, over a stand-in body and window.
;;; The test-op runs every listed suite package in a single Parachute run.

(defsystem "clim-clog/tests"
  :description "Headless contract tests for the clim-clog medium protocol."
  :depends-on ("clim-clog" "parachute")
  :pathname "tests"
  :components ((:file "medium-metrics")
               (:file "medium-paint")
               (:file "wheel-input")
               (:file "recording-context")
               (:file "measured-text" :depends-on ("recording-context"))
               (:file "ellipse-arcs" :depends-on ("recording-context"))
               (:file "mock-connection")
               (:file "coalesced-frames" :depends-on ("mock-connection"))
               (:file "input-order" :depends-on ("mock-connection"))
               (:file "window-resize"
                :depends-on ("recording-context" "mock-connection"))
               (:file "reader-thread" :depends-on ("mock-connection")))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (let* ((suites '("CLIM-CLOG/TESTS/MEDIUM-METRICS"
                                     "CLIM-CLOG/TESTS/MEDIUM-PAINT"
                                     "CLIM-CLOG/TESTS/WHEEL-INPUT"
                                     "CLIM-CLOG/TESTS/MEASURED-TEXT"
                                     "CLIM-CLOG/TESTS/ELLIPSE-ARCS"
                                     "CLIM-CLOG/TESTS/COALESCED-FRAMES"
                                     "CLIM-CLOG/TESTS/INPUT-ORDER"
                                     "CLIM-CLOG/TESTS/WINDOW-RESIZE"
                                     "CLIM-CLOG/TESTS/READER-THREAD"))
                           (packages
                             (mapcar
                              (lambda (name)
                                (let ((pkg (or (find-package name)
                                               (error "clim-clog/tests: suite package ~a is not loaded."
                                                      name))))
                                  (unless (uiop:symbol-call :parachute :package-tests pkg)
                                    (error "clim-clog/tests: suite package ~a defines no test."
                                           name))
                                  pkg))
                              suites))
                           (result (uiop:symbol-call :parachute :test packages)))
                      (when (uiop:symbol-call :parachute :results-with-status
                                              :failed result)
                        (error "clim-clog/tests: one or more tests failed.")))))
