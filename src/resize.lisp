;;;; resize.lisp
;;;;
;;;; Following a resize of the browser window that holds a port.
;;;;
;;;; A port created with a CLOG body binds a handler on the body's window,
;;;; under an event name of its own, and installs a page script.  The script
;;;; debounces the browser's resize events and, once the window has kept one
;;;; size for *RESIZE-DEBOUNCE-MILLISECONDS*, sends that inner size over the
;;;; websocket.  Destroying the port removes both the handler and the script's
;;;; listener.  Each step then runs on its own thread:
;;;;   - the handler runs on CLOG's websocket reader thread, in arrival order
;;;;     with canvas input (see CANVAS-MESSAGE-HANDLER in input.lisp).  It
;;;;     parses the size and queues one BROWSER-WINDOW-RESIZE-EVENT for the
;;;;     graft on the port's queue, and neither draws nor queries the browser;
;;;;   - the port I/O thread records the size on the port and the graft, then
;;;;     dispatches one event to the top-level sheet of each frame on the port
;;;;     for which FRAME-FOLLOWS-BROWSER-WINDOW-P is true;
;;;;   - each such frame's own thread handles its event by calling
;;;;     ADJUST-FRAME-TO-BROWSER-WINDOW, which lays the frame out and so resizes
;;;;     its visible and offscreen canvases (SET-MIRROR-GEOMETRY in port.lisp).

(in-package #:clim-clog)

(defparameter *resize-debounce-milliseconds* 150
  "Milliseconds the browser window keeps one size before that size is sent.
Read when a port binds its resize handler.")

(defun browser-resize-event-name (port)
  "The name of the jQuery event PORT's page script triggers on the CLOG
window.  Each port has its own, so ports sharing a body do not share a
handler."
  (format nil "clim-clog-resize-~(~a~)" (slot-value port 'id)))

;;; Not a WINDOW-CONFIGURATION-EVENT: McCLIM handles that class as a report
;;; that the window system has already resized the mirror, so it records the
;;; event region as the mirror geometry and skips SET-MIRROR-GEOMETRY.  Here
;;; the canvases still have to be resized, to the size the application's
;;; specialized ADJUST-FRAME-TO-BROWSER-WINDOW method sets.
(defclass browser-window-resize-event (window-event)
  ()
  (:documentation "The browser window holding a port changed to the size of
the event's native region, (0 0 width height) in CSS pixels.  One is queued
for the port's graft.  The port I/O thread then dispatches one to the top-level
sheet of each frame of the port's frame managers when:
  - the frame's top-level sheet exists;
  - that sheet has a mirror;
  - FRAME-FOLLOWS-BROWSER-WINDOW-P is true of the frame.
HANDLE-EVENT on that sheet calls ADJUST-FRAME-TO-BROWSER-WINDOW on the
frame's thread."))

(defmethod event-type ((event browser-window-resize-event))
  :browser-window-resize)

(defgeneric adjust-frame-to-browser-window (frame width height)
  (:documentation "Resize FRAME after the browser window holding its port
changed to WIDTH by HEIGHT CSS pixels.

Called on FRAME's own thread, from HANDLE-EVENT in the frame's event loop,
once for each size the page script sends to FRAME's port, and only when
FRAME-FOLLOWS-BROWSER-WINDOW-P is true of FRAME.  In that call, WIDTH and HEIGHT are the size the port
I/O thread stored in CLOG-PORT-WIDTH, CLOG-PORT-HEIGHT, and the graft's region
before queuing the event.  A later resize can change those values before
FRAME's thread handles the event, and a method that adjusts the size passes
the next method values the port does not hold.

The default method calls (LAYOUT-FRAME FRAME WIDTH HEIGHT), which resizes the
top-level sheet, its visible and offscreen canvases, and the panes.

Specialize it on an application frame class to apply a minimum size or to
resize content outside the canvases.  Call CALL-NEXT-METHOD with the size the
frame should take, for example
  (call-next-method frame (max 800 width) (max 600 height))."))

(defmethod adjust-frame-to-browser-window ((frame application-frame)
                                           width height)
  (layout-frame frame width height))

(defgeneric frame-follows-browser-window-p (frame)
  (:documentation "True when FRAME is sized to the browser window, so that a
resize of the window reaches ADJUST-FRAME-TO-BROWSER-WINDOW.  Called on the
port I/O thread.

The default methods answer true for a STANDARD-APPLICATION-FRAME with no
calling frame, and false for every other frame:
  - a menu frame, which is not a STANDARD-APPLICATION-FRAME;
  - a dialog run in its own window, whose FRAME-CALLING-FRAME is set;
  - a window that OPEN-WINDOW-STREAM opened.
Specialize it to have another frame follow the window, or a top-level frame
keep its size."))

(defmethod frame-follows-browser-window-p ((frame application-frame))
  nil)

(defmethod frame-follows-browser-window-p
    ((frame standard-application-frame))
  (null (frame-calling-frame frame)))

(defmethod frame-follows-browser-window-p ((frame climi::a-window-stream))
  nil)

(defun resize-relay-script (window event-name)
  "JavaScript that adds a resize listener to the browser window.  Once the
window has kept one size for *RESIZE-DEBOUNCE-MILLISECONDS*, the listener
triggers EVENT-NAME on WINDOW with the data \"innerWidth:innerHeight\".  The
listener is stored under EVENT-NAME in window.climClogResize, where
RESIZE-RELAY-REMOVAL-SCRIPT finds it."
  (format nil "(function(){var r=window.climClogResize=window.climClogResize||{};~
var f=function(){clearTimeout(f.timer);~
f.timer=setTimeout(function(){~A.trigger('~A',[window.innerWidth+':'+window.innerHeight]);},~D);};~
r['~A']=f;window.addEventListener('resize',f);})()"
          (clog:jquery window)
          event-name
          *resize-debounce-milliseconds*
          event-name))

(defun resize-relay-removal-script (event-name)
  "JavaScript that removes the resize listener RESIZE-RELAY-SCRIPT stored
under EVENT-NAME, and cancels its pending trigger."
  (format nil "(function(){var r=window.climClogResize,f=r&&r['~A'];~
if(f){window.removeEventListener('resize',f);clearTimeout(f.timer);~
delete r['~A'];}})()"
          event-name
          event-name))

(defun parse-browser-window-size (data)
  "Parse the \"WIDTH:HEIGHT\" string DATA the page script sends.  Return the
width and height as positive integers, or NIL when DATA is not of that form."
  (let ((colon (and (stringp data) (position #\: data))))
    (when colon
      (let ((w (parse-integer data :end colon :junk-allowed t))
            (h (parse-integer data :start (1+ colon) :junk-allowed t)))
        (when (and w h (plusp w) (plusp h))
          (values w h))))))

(defun queue-browser-window-resize (port data)
  "Queue a BROWSER-WINDOW-RESIZE-EVENT for PORT's graft from the page script's
DATA.  Touch no sheet, send nothing to the browser, and never wait: this runs
on CLOG's websocket reader thread, which also delivers the answers a wait
would need, and sheet changes belong to the port I/O and frame threads.
Without a graft, set only the port's size, since no frame exists to resize.
Ignore DATA that PARSE-BROWSER-WINDOW-SIZE rejects."
  (multiple-value-bind (width height) (parse-browser-window-size data)
    (when width
      (let ((graft (first (port-grafts port))))
        (if graft
            (clog-enqueue-event
             port
             (make-instance 'browser-window-resize-event
                            :sheet graft
                            :region (make-bounding-rectangle 0 0 width height)))
            (setf (clog-port-width port) width
                  (clog-port-height port) height))))))

(defun %port-window (port)
  "The CLOG window of PORT's body, or NIL when the body is not a CLOG-BODY or
has no window object."
  (let ((body (clog-port-body port)))
    (and (typep body 'clog:clog-body)
         (handler-case (clog:window body)
           (unbound-slot () nil)))))

(defun install-browser-resize-handler (port)
  "Bind PORT's resize handler on the window of PORT's CLOG body, have it run
on the websocket reader thread, and install the page script that debounces
the browser's resize events.  Do nothing when %PORT-WINDOW finds no window."
  (let ((window (%port-window port))
        (event-name (browser-resize-event-name port)))
    (when window
      (clog:set-on-event-with-data
       window event-name
       (lambda (obj data)
         (declare (ignore obj))
         (queue-browser-window-resize port data)))
      (register-reader-thread-hooks window (list event-name))
      (clog:js-execute window (resize-relay-script window event-name))))
  (values))

(defun remove-browser-resize-handler (port)
  "Undo INSTALL-BROWSER-RESIZE-HANDLER: unbind PORT's resize handler, stop
handling it on the reader thread, and remove the page script's listener.
Log, rather than signal, an error from a connection that is already gone."
  (let ((window (%port-window port))
        (event-name (browser-resize-event-name port)))
    (when window
      (handler-case
          (progn
            (forget-reader-thread-hooks window (list event-name))
            (clog:set-on-event-with-data window event-name nil)
            (clog:js-execute window (resize-relay-removal-script event-name)))
        (error (e)
          (format *error-output* "~&[clim-clog resize] ~a~%" e)))))
  (values))

(defmethod destroy-port :before ((port clog-port))
  (remove-browser-resize-handler port))

;;; Runs on the port I/O thread.  An event for the graft records the size and
;;; fans out; an event for a top-level sheet goes to that sheet's queue.
(defmethod distribute-event ((port clog-port)
                             (event browser-window-resize-event))
  (let ((sheet (event-sheet event)))
    (if (typep sheet 'graft)
        (let ((region (window-event-native-region event)))
          (multiple-value-bind (width height) (bounding-rectangle-size region)
            (setf (clog-port-width port) width
                  (clog-port-height port) height)
            ;; (SETF SHEET-REGION) signals on a graft, which has no sheet
            ;; transformation, so the graft's rectangle is updated in place.
            (setf (rectangle-edges* (sheet-region sheet))
                  (values 0 0 width height)))
          (dolist (manager (frame-managers port))
            (dolist (frame (frame-manager-frames manager))
              (let ((top (frame-top-level-sheet frame)))
                (when (and top
                           (sheet-direct-mirror top)
                           (frame-follows-browser-window-p frame))
                  (dispatch-event
                   top
                   (make-instance 'browser-window-resize-event
                                  :sheet top
                                  :region region)))))))
        (call-next-method))))

;;; A menu's top-level sheet reports the frame that opened the menu as its
;;; PANE-FRAME, so only the frame whose own top-level sheet SHEET is resizes.
(defmethod handle-event ((sheet top-level-sheet-mixin)
                         (event browser-window-resize-event))
  (let ((frame (and (typep sheet 'pane) (pane-frame sheet))))
    (when (and frame (eq sheet (frame-top-level-sheet frame)))
      (multiple-value-bind (width height)
          (bounding-rectangle-size (window-event-native-region event))
        (adjust-frame-to-browser-window frame width height)))))
