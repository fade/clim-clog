;;;; tests/window-resize.lisp
;;;;
;;;; Headless contract tests for how a clim-clog port follows a resize of the
;;;; browser window that holds it.
;;;;
;;;; These need no browser.  A stand-in body, window, and canvas take the place
;;;; of a tab: they record the scripts and event handlers CLOG would send, and
;;;; each canvas keeps the properties set on it.  A frame is adopted on a port
;;;; built over that body, the handler the port binds on the window is called
;;;; with the data a browser sends, and the events the frame receives are
;;;; handled on the test thread in place of a running command loop.

(defpackage #:clim-clog/tests/window-resize
  (:use #:clim #:clim-lisp)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true)
  (:import-from #:clim-clog/tests/recording-context
                #:make-recording-context
                #:recorded-scripts))

(in-package #:clim-clog/tests/window-resize)

;;; ------------------------------------------------------------------
;;; Stand-in CLOG objects.

(defclass stand-in-element ()
  ((scripts :initform '() :accessor %scripts
            :documentation "The scripts sent to the object, newest first.")
   (handlers :initform '() :accessor %handlers
             :documentation "An alist of event name to the function CLOG
would call with the browser's event data."))
  (:documentation "A CLOG object that records scripts and event handlers in
place of a browser connection."))

(defmethod clog:js-execute ((obj stand-in-element) script)
  (push script (%scripts obj))
  obj)

(defmethod clog:js-query ((obj stand-in-element) script &key default-answer)
  (push script (%scripts obj))
  default-answer)

;;; Every CLOG SET-ON-* function binds its handler through SET-EVENT, which
;;; is given the function CLOG calls with the event data.
(defmethod clog::set-event ((obj stand-in-element) event handler
                            &key &allow-other-keys)
  (setf (%handlers obj)
        (cons (cons event handler)
              (remove event (%handlers obj) :key #'car :test #'equal)))
  obj)

(defclass stand-in-window (stand-in-element clog:clog-window) ())

(defclass stand-in-document (stand-in-element clog:clog-document) ())

(defclass stand-in-canvas (stand-in-element clog:clog-canvas)
  ((properties :initform (make-hash-table :test 'equal) :reader %properties)
   (writes :initform 0 :accessor %writes
           :documentation "How many times a property was set on the canvas."))
  (:documentation "A canvas that keeps the properties set on it and counts
the writes."))

(defmethod clog:property ((obj stand-in-canvas) name &key default-answer)
  (gethash name (%properties obj) default-answer))

(defmethod (setf clog:property) (value (obj stand-in-canvas) name)
  (incf (%writes obj))
  (setf (gethash name (%properties obj)) (princ-to-string value)))

(defmethod clog:create-context2d ((obj stand-in-canvas))
  (make-recording-context))

(defclass stand-in-body (stand-in-element clog:clog-body)
  ((canvases :initform '() :accessor %canvases
             :documentation "The canvases created under the body, newest
first.")))

(defmethod clog:create-canvas ((obj stand-in-body)
                               &key (width 300) (height 150)
                               &allow-other-keys)
  (let ((canvas (make-instance 'stand-in-canvas
                               :connection-id nil
                               :html-id (format nil "canvas~D"
                                                (length (%canvases obj))))))
    (setf (clog:property canvas "width") width
          (clog:property canvas "height") height)
    (push canvas (%canvases obj))
    canvas))

(defun make-stand-in-body ()
  "A stand-in body with its own stand-in window and document."
  (make-instance 'stand-in-body
                 :connection-id nil
                 :html-id 0
                 :window (make-instance 'stand-in-window
                                        :connection-id nil
                                        :html-id "window")
                 :html-document (make-instance 'stand-in-document
                                               :connection-id nil
                                               :html-id "document")))

(defun canvas-size (canvas)
  "CANVAS's width and height properties, as a list of two strings."
  (list (clog:property canvas "width") (clog:property canvas "height")))

;;; ------------------------------------------------------------------
;;; Frames that record each resize they are told of.

(define-application-frame resize-test-frame ()
  ((resizes :initform '() :accessor frame-resizes
            :documentation "Each (WIDTH HEIGHT) the frame was told of,
newest first."))
  (:pane (make-pane 'basic-pane)))

(defmethod clim-clog:adjust-frame-to-browser-window :before
    ((frame resize-test-frame) width height)
  (push (list width height) (frame-resizes frame)))

(define-application-frame floored-test-frame (resize-test-frame)
  ()
  (:pane (make-pane 'basic-pane)))

(defmethod clim-clog:adjust-frame-to-browser-window
    ((frame floored-test-frame) width height)
  (call-next-method frame (max 800 width) (max 600 height)))

(defclass painted-pane (basic-pane)
  ()
  (:documentation "A pane that draws a rectangle on every repaint."))

(defmethod handle-repaint ((pane painted-pane) region)
  (declare (ignore region))
  (draw-rectangle* pane 10 10 110 60 :ink +red+))

(define-application-frame painted-test-frame (resize-test-frame)
  ()
  (:pane (make-pane 'painted-pane)))

;;; ------------------------------------------------------------------
;;; Driving a resize.

(defun call-with-tab (frame-class function)
  "Make a stand-in body, a fresh port over it, and a frame of FRAME-CLASS
adopted on that port at 1280x600.  Call FUNCTION with three arguments:
  - the frame;
  - the port;
  - the body.
Destroy the port after."
  (let* ((body (make-stand-in-body))
         (port (find-port :server-path
                          (list :clog :body body :width 1280 :height 600))))
    (unwind-protect
         (let ((frame (make-application-frame
                       frame-class
                       :frame-manager (find-frame-manager :port port)
                       :width 1280 :height 600)))
           (funcall function frame port body))
      (destroy-port port))))

(defun resize-handler (body)
  "The handler bound on BODY's window for the resize, or NIL."
  (cdr (first (%handlers (clog:window body)))))

(defun resize-tab (frame body data)
  "Call the window's resize handler with DATA as CLOG would, then handle the
events FRAME receives until FRAME is told of the resize or a second passes.
Return true when FRAME was told."
  (let ((handler (resize-handler body))
        (told (length (frame-resizes frame)))
        (sheet (frame-top-level-sheet frame))
        (deadline (+ (get-internal-real-time) internal-time-units-per-second)))
    (when handler
      (funcall handler data)
      (loop
        (let ((event (event-read-no-hang sheet)))
          (cond (event
                 (handle-event (event-sheet event) event))
                ((> (length (frame-resizes frame)) told)
                 (return t))
                ((> (get-internal-real-time) deadline)
                 (return nil))
                (t
                 (sleep 0.01))))))))

(defun mirror-canvases (frame)
  "The visible and offscreen canvases of FRAME's top-level sheet."
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (list (clim-clog::clog-mirror-canvas mirror)
          (clim-clog::clog-mirror-offscreen-canvas mirror))))

(defun handle-pending-events (frame)
  "Handle the events already queued for FRAME, without waiting for more."
  (let ((sheet (frame-top-level-sheet frame)))
    (loop for event = (event-read-no-hang sheet)
          while event
          do (handle-event (event-sheet event) event))))

(defun mirror-script-counts (frame)
  "Two values for FRAME's mirror: the number of blits sent to the visible
context, and the number of scripts sent to the offscreen context."
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (values (count-if (lambda (script) (search "drawImage(" script))
                      (recorded-scripts (clim-clog::clog-mirror-context mirror)))
            (length (recorded-scripts
                     (clim-clog::clog-mirror-offscreen-context mirror))))))

;;; ------------------------------------------------------------------
;;; The tests.

(define-test the-port-binds-one-resize-handler-on-the-window
  (call-with-tab 'resize-test-frame
                 (lambda (frame port body)
                   (declare (ignore frame port))
                   (is = 1 (length (%handlers (clog:window body))))
                   (true (resize-handler body)))))

(define-test the-frame-follows-a-window-resize
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (true (resize-tab frame body "1400:900"))
     (is equal '((1400 900)) (frame-resizes frame))
     (is equal '(1400 900)
         (multiple-value-list
          (bounding-rectangle-size (frame-top-level-sheet frame))))
     (is equal '(1400 900)
         (list (clim-clog::clog-port-width port)
               (clim-clog::clog-port-height port)))
     (is equal '(1400 900)
         (multiple-value-list
          (bounding-rectangle-size (find-graft :port port))))
     (dolist (canvas (mirror-canvases frame))
       (is equal '("1400" "900") (canvas-size canvas))))))

(define-test the-application-can-floor-the-frame-size
  (call-with-tab
   'floored-test-frame
   (lambda (frame port body)
     (true (resize-tab frame body "700:500"))
     (is equal '((700 500)) (frame-resizes frame))
     (is equal '(700 500)
         (list (clim-clog::clog-port-width port)
               (clim-clog::clog-port-height port)))
     (is equal '(800 600)
         (multiple-value-list
          (bounding-rectangle-size (frame-top-level-sheet frame))))
     (dolist (canvas (mirror-canvases frame))
       (is equal '("800" "600") (canvas-size canvas))))))

(define-test a-resize-presents-once
  (call-with-tab
   'painted-test-frame
   (lambda (frame port body)
     (declare (ignore port))
     (enable-frame frame)
     (handle-pending-events frame)
     (multiple-value-bind (blits drawn) (mirror-script-counts frame)
       (true (resize-tab frame body "1400:900"))
       (handle-pending-events frame)
       (multiple-value-bind (blits-after drawn-after) (mirror-script-counts frame)
         (true (> drawn-after drawn))
         (is = 1 (- blits-after blits)))))))

(define-test the-browser-debounces-the-resize
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (declare (ignore frame port))
     (let ((relay (find "addEventListener('resize'"
                        (%scripts (clog:window body))
                        :test #'search)))
       (true relay)
       (when relay
         (true (search "clearTimeout(" relay))
         (true (search (format nil "},~D)"
                               clim-clog:*resize-debounce-milliseconds*)
                       relay))
         (is = 150 clim-clog:*resize-debounce-milliseconds*))))))

(define-test an-unchanged-geometry-leaves-the-canvases-alone
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (declare (ignore body))
     (let ((top (frame-top-level-sheet frame))
           (canvases (mirror-canvases frame)))
       (flet ((set-geometry (width height)
                (clim-backend:set-mirror-geometry
                 port top (make-bounding-rectangle 0 0 width height)))
              (writes ()
                (mapcar #'%writes canvases)))
         (set-geometry 1400 900)
         (let ((after-resize (writes)))
           (set-geometry 1400 900)
           (is equal after-resize (writes)))
         (dolist (canvas canvases)
           (is equal '("1400" "900") (canvas-size canvas))))))))

(define-test the-port-source-compiles-without-full-warnings
  (let ((warnings '()))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (handler-bind ((warning
                       (lambda (w)
                         (unless (typep w 'style-warning)
                           (push (princ-to-string w) warnings))
                         (muffle-warning w))))
        (with-compilation-unit (:override t)
          (compile-file (asdf:system-relative-pathname "clim-clog"
                                                       "src/port.lisp")
                        :output-file fasl))))
    (is equal '() warnings)))
