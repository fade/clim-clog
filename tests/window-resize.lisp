;;;; tests/window-resize.lisp
;;;;
;;;; Headless contract tests for how a clim-clog port follows a resize of the
;;;; browser window that holds it.
;;;;
;;;; These need no browser.  A stand-in body, window, and canvas take the place
;;;; of a tab: they record every script and every event binding CLOG would
;;;; send, and each canvas keeps the properties set on it.  SETTLE-RESIZE plays
;;;; the browser's part for one settled resize: each resize listener the
;;;; window's scripts installed and did not remove triggers its event, and
;;;; each jQuery binding of that event sends the data to the handler CLOG
;;;; holds for it.  The events a frame receives are handled on the test
;;;; thread in place of a running command loop.  The ordering test instead
;;;; runs on a mock connection (see mock-connection.lisp), so that CLOG's own
;;;; message entry point decides which thread runs the handler.

(defpackage #:clim-clog/tests/window-resize
  (:use #:clim #:clim-lisp)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false)
  (:import-from #:clim-clog/tests/recording-context
                #:make-recording-context
                #:recorded-scripts)
  (:import-from #:clim-clog/tests/mock-connection
                #:make-mock-socket
                #:mock-socket-id
                #:release-mock-socket
                #:release-mock-port
                #:stop-port-event-loop
                #:deliver-browser-message))

(in-package #:clim-clog/tests/window-resize)

;;; ------------------------------------------------------------------
;;; Stand-in CLOG objects.

(defclass stand-in-element ()
  ((scripts :initform '() :accessor %scripts
            :documentation "The scripts sent to the object, newest first.")
   (binds :initform '() :accessor %binds
          :documentation "Every (EVENT . HANDLER) passed to SET-EVENT, newest
first.  HANDLER is the function CLOG calls with the browser's event data, or
NIL for an unbind."))
  (:documentation "A CLOG object that records scripts and event bindings in
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
  (push (cons event handler) (%binds obj))
  obj)

(defun bound-handler (obj event)
  "The handler CLOG holds for EVENT on OBJ: the one given by the newest bind
of EVENT, or NIL when that bind was an unbind or there is none."
  (cdr (assoc event (%binds obj) :test #'equal)))

(defun jquery-binding-count (obj event)
  "How many jQuery handlers for EVENT the binds left on OBJ.  Each bind with
a handler adds one with .on, and an unbind removes them all with .off."
  (let ((count 0))
    (loop for (name . handler) in (%binds obj)
          when (equal name event)
            do (if handler
                   (incf count)
                   (return)))
    count))

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

(defun resize-listeners (window)
  "The resize listeners WINDOW's scripts installed and did not remove, oldest
first, each a list of the jQuery target and the event name it triggers.  A
script calling addEventListener('resize', ...) whose callback triggers an
event installs one.  A script calling removeEventListener('resize', ...) for
a listener stored under that event name removes it."
  (let ((listeners '()))
    (dolist (script (reverse (%scripts window)) (reverse listeners))
      (cond ((search "addEventListener('resize'" script)
             (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings
                  "(\\$\\([^)]*\\))\\.trigger\\('([^']+)'" script)
               (when match
                 (push (list (aref groups 0) (aref groups 1)) listeners))))
            ((search "removeEventListener('resize'" script)
             (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings "\\['([^']+)'\\]" script)
               (when match
                 (setf listeners
                       (remove (aref groups 0) listeners
                               :key #'second :test #'equal :count 1)))))))))

(defun settle-resize (window data)
  "Play one settled browser resize to DATA on WINDOW.  Each listener whose
trigger target is WINDOW's jQuery object triggers its event, and each jQuery
binding of that event calls the handler CLOG holds for it with DATA.  Return
the number of handler calls."
  (let ((calls 0))
    (loop for (target event) in (resize-listeners window)
          for handler = (bound-handler window event)
          when (and handler (equal target (clog:jquery window)))
            do (loop repeat (jquery-binding-count window event)
                     do (funcall handler data)
                        (incf calls)))
    calls))

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

(defun resize-tab (frame body data)
  "Settle a browser resize to DATA on BODY's window, then handle the events
FRAME receives until FRAME is told of the resize or a second passes.  Return
true when FRAME was told."
  (let ((told (length (frame-resizes frame)))
        (sheet (frame-top-level-sheet frame))
        (deadline (+ (get-internal-real-time) internal-time-units-per-second)))
    (settle-resize (clog:window body) data)
    (loop
      (let ((event (event-read-no-hang sheet)))
        (cond (event
               (handle-event (event-sheet event) event))
              ((> (length (frame-resizes frame)) told)
               (return t))
              ((> (get-internal-real-time) deadline)
               (return nil))
              (t
               (sleep 0.01)))))))

(defun mirror-canvases (sheet)
  "The visible and offscreen canvases of SHEET's mirror."
  (let ((mirror (sheet-direct-mirror sheet)))
    (list (clim-clog::clog-mirror-canvas mirror)
          (clim-clog::clog-mirror-offscreen-canvas mirror))))

(defun handle-pending-events (sheet)
  "Handle the events already queued for SHEET, without waiting for more."
  (loop for event = (event-read-no-hang sheet)
        while event
        do (handle-event (event-sheet event) event)))

(defun mirror-script-counts (frame)
  "Two values for FRAME's mirror: the number of blits sent to the visible
context, and the number of scripts sent to the offscreen context."
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (values (count-if (lambda (script) (search "drawImage(" script))
                      (recorded-scripts (clim-clog::clog-mirror-context mirror)))
            (length (recorded-scripts
                     (clim-clog::clog-mirror-offscreen-context mirror))))))

(defun port-size (port)
  "PORT's width and height, as a list."
  (list (clim-clog::clog-port-width port) (clim-clog::clog-port-height port)))

;;; ------------------------------------------------------------------
;;; The tests.

(define-test the-port-binds-one-resize-handler-on-the-window
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (declare (ignore frame port))
     (let* ((window (clog:window body))
            (listeners (resize-listeners window)))
       (is = 1 (length listeners))
       (is = 1 (count-if #'cdr (%binds window)))
       (when listeners
         (destructuring-bind (target event) (first listeners)
           (is equal (clog:jquery window) target)
           (true (bound-handler window event))
           (is = 1 (jquery-binding-count window event))))))))

(define-test each-port-on-a-body-hears-each-resize-once
  (let* ((body (make-stand-in-body))
         (window (clog:window body))
         (ports (loop for width in '(1280 1100)
                      collect (make-instance
                               'clim-clog:clog-port
                               :server-path (list :clog :body body
                                                  :width width :height 600)))))
    (unwind-protect
         (destructuring-bind (first-port second-port) ports
           (is = 2 (settle-resize window "1400:900"))
           (is equal '(1400 900) (port-size first-port))
           (is equal '(1400 900) (port-size second-port))
           (destroy-port first-port)
           (is = 1 (settle-resize window "1000:700"))
           (is equal '(1400 900) (port-size first-port))
           (is equal '(1000 700) (port-size second-port)))
      (dolist (port ports)
        (ignore-errors (destroy-port port))))))

(define-test the-frame-follows-a-window-resize
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (true (resize-tab frame body "1400:900"))
     (is equal '((1400 900)) (frame-resizes frame))
     (is equal '(1400 900)
         (multiple-value-list
          (bounding-rectangle-size (frame-top-level-sheet frame))))
     (is equal '(1400 900) (port-size port))
     (is equal '(1400 900)
         (multiple-value-list
          (bounding-rectangle-size (find-graft :port port))))
     (dolist (canvas (mirror-canvases (frame-top-level-sheet frame)))
       (is equal '("1400" "900") (canvas-size canvas))))))

(define-test the-application-can-floor-the-frame-size
  (call-with-tab
   'floored-test-frame
   (lambda (frame port body)
     (true (resize-tab frame body "700:500"))
     (is equal '((700 500)) (frame-resizes frame))
     (is equal '(700 500) (port-size port))
     (is equal '(800 600)
         (multiple-value-list
          (bounding-rectangle-size (frame-top-level-sheet frame))))
     (dolist (canvas (mirror-canvases (frame-top-level-sheet frame)))
       (is equal '("800" "600") (canvas-size canvas))))))

(define-test menus-and-dialogs-keep-their-size
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (enable-frame frame)
     (handle-pending-events (frame-top-level-sheet frame))
     (let* ((manager (find-frame-manager :port port))
            (*application-frame* frame)
            (menu (climi::make-menu-frame
                   (make-pane-1 manager frame 'basic-pane
                                :width 120 :height 80)))
            (dialog (make-application-frame 'resize-test-frame
                                            :calling-frame frame
                                            :frame-manager manager
                                            :width 300 :height 200)))
       (adopt-frame manager menu)
       (enable-frame menu)
       (enable-frame dialog)
       (true (clim-clog:frame-follows-browser-window-p frame))
       (false (clim-clog:frame-follows-browser-window-p menu))
       (false (clim-clog:frame-follows-browser-window-p dialog))
       (let* ((sheets (list (frame-top-level-sheet menu)
                            (frame-top-level-sheet dialog)))
              (sizes (loop for sheet in sheets
                           collect (multiple-value-list
                                    (bounding-rectangle-size sheet))))
              (canvases (loop for sheet in sheets
                              collect (mapcar #'canvas-size
                                              (mirror-canvases sheet)))))
         (true (resize-tab frame body "1400:900"))
         (sleep 0.1)
         (mapc #'handle-pending-events sheets)
         (is equal '((1400 900)) (frame-resizes frame))
         (is equal '() (frame-resizes dialog))
         (is equal sizes
             (loop for sheet in sheets
                   collect (multiple-value-list
                            (bounding-rectangle-size sheet))))
         (is equal canvases
             (loop for sheet in sheets
                   collect (mapcar #'canvas-size
                                   (mirror-canvases sheet)))))))))

(define-test a-resize-presents-once
  (call-with-tab
   'painted-test-frame
   (lambda (frame port body)
     (declare (ignore port))
     (enable-frame frame)
     (handle-pending-events (frame-top-level-sheet frame))
     (multiple-value-bind (blits drawn) (mirror-script-counts frame)
       (true (resize-tab frame body "1400:900"))
       (handle-pending-events (frame-top-level-sheet frame))
       (multiple-value-bind (blits-after drawn-after) (mirror-script-counts frame)
         (true (> drawn-after drawn))
         (is = 1 (- blits-after blits)))))))

(define-test the-browser-debounces-the-resize
  (let ((clim-clog:*resize-debounce-milliseconds* 40))
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
           (true (search "setTimeout(" relay))
           (true (search "},40)" relay))))))))

(define-test an-unchanged-geometry-leaves-the-canvases-alone
  (call-with-tab
   'resize-test-frame
   (lambda (frame port body)
     (declare (ignore body))
     (let* ((top (frame-top-level-sheet frame))
            (canvases (mirror-canvases top)))
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

(defun window-resize-hook (connection-id)
  "The CLOG hook string of a handler bound on CONNECTION-ID's window, or NIL."
  (let ((table (clog-connection:get-connection-data connection-id)))
    (loop for key being the hash-keys of table using (hash-value value)
          when (and (stringp key)
                    (functionp value)
                    (eql 0 (search "window:" key)))
            return key)))

(defun queued-event-types (port)
  "The types of the events in PORT's clog-port queue, oldest first."
  (clim-sys:with-lock-held ((clim-clog::clog-port-event-lock port))
    (mapcar #'type-of (clim-clog::clog-port-event-queue port))))

(define-test a-resize-is-queued-in-arrival-order
  (let* ((socket (make-mock-socket))
         (id (mock-socket-id socket))
         (body (make-instance 'clog:clog-body
                              :connection-id id
                              :html-id "body"
                              :window (make-instance 'clog:clog-window
                                                     :connection-id id
                                                     :html-id "window")))
         (port nil))
    (unwind-protect
         (let (frame canvas-id hook)
           (setf port (make-instance 'clim-clog:clog-port
                                     :server-path (list :clog :body body
                                                        :width 400
                                                        :height 300))
                 frame (make-application-frame
                        'resize-test-frame
                        :frame-manager (find-frame-manager :port port)
                        :width 400 :height 300))
           (enable-frame frame)
           (stop-port-event-loop port)
           (setf (clim-clog::clog-port-event-queue port) '()
                 canvas-id (clog:html-id
                            (clim-clog::clog-mirror-canvas
                             (sheet-direct-mirror
                              (frame-top-level-sheet frame))))
                 hook (window-resize-hook id))
           (true hook)
           (when hook
             (true (clim-clog::canvas-event-hook-p
                    (clog-connection:get-connection-data id) hook))
             (deliver-browser-message socket
                                      (format nil "E:~a 1400:900" hook))
             (is equal '(clim-clog:browser-window-resize-event)
                 (queued-event-types port))
             (deliver-browser-message
              socket
              (format nil "E:~a:mousedown ~
10:20:10:20:1:false:false:false:false:10:20:10:20"
                      canvas-id))
             (is equal '(clim-clog:browser-window-resize-event
                         pointer-button-press-event)
                 (queued-event-types port))))
      (when port (release-mock-port port))
      (release-mock-socket socket))))
