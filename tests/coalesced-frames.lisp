;;;; tests/coalesced-frames.lisp
;;;;
;;;; Contract tests for how often a clim-clog mirror presents its offscreen
;;;; buffer: one input event yields one presented frame, and the frame it
;;;; presents is the finished one.
;;;;
;;;; A frame runs on a clog-port whose body lives on a mock connection (see
;;;; mock-connection.lisp), so every script CLOG would send to a browser is
;;;; recorded.  A presented frame is a drawImage script: the blit of the
;;;; offscreen canvas onto the visible one.  Each test drives a pane on the
;;;; calling thread in one of four ways:
;;;;   - it hands one event to HANDLE-EVENT, as the frame's command loop does;
;;;;   - it redisplays the frame's panes, as the command loop does after a
;;;;     command;
;;;;   - it writes to the pane outside any event;
;;;;   - it accepts a string typed into an interactor.
;;;; Each test then reads the scripts that step produced.

(defpackage #:clim-clog/tests/coalesced-frames
  (:use #:clim #:clim-lisp #:clim-backend
        #:clim-clog/tests/mock-connection)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:clim-clog/tests/coalesced-frames)

(defparameter *line-count* 20
  "Lines the test pane writes on each redraw.")

(defvar *socket* nil
  "The mock socket of the frame under test.")

(defvar *blits-before-wait* nil
  "Blits sent before the button-press handler starts to wait for input.")

(defclass redrawing-pane (application-pane)
  ()
  (:documentation "A pane that redisplays on a wheel event, as a scrolling
pane does, and rewrites its lines in place on a keystroke, as a text editor
pane does."))

(defun display-lines (frame pane)
  "Write *LINE-COUNT* lines to PANE."
  (declare (ignore frame))
  (dotimes (i *line-count*)
    (format pane "line ~d of the pane~%" i)))

(defmethod handle-event ((pane redrawing-pane) (event pointer-scroll-event))
  (redisplay-frame-pane (pane-frame pane) pane :force-p t))

(defmethod handle-event ((pane redrawing-pane) (event key-press-event))
  (window-clear pane)
  (dotimes (i *line-count*)
    (format pane "edited line ~d~%" i))
  (finish-output pane))

(defmethod handle-event ((pane redrawing-pane) (event pointer-button-press-event))
  (format pane "waiting~%")
  (finish-output pane)
  ;; Wait briefly for input, as a modal loop run from a handler does.  The
  ;; drawing so far must be on screen before the wait.
  (climi::queue-read-or-wait (climi::sheet-event-queue pane) 0.01 nil)
  (setf *blits-before-wait* (blit-count (sent-scripts *socket*))))

(define-application-frame redrawing-frame ()
  ()
  (:panes (lines (make-pane 'redrawing-pane
                            :display-function 'display-lines
                            :scroll-bars nil)))
  (:layouts (default lines)))

(defun call-with-redrawing-pane (function)
  "Call FUNCTION with the mock socket and the enabled frame's pane, with
*SOCKET* bound to the socket, then tear the frame, port, and socket down."
  (let* ((socket (make-mock-socket))
         (*socket* socket)
         (port nil)
         (frame nil))
    (unwind-protect
         (progn
           (setf port (make-mock-port socket :width 800 :height 600)
                 frame (make-application-frame 'redrawing-frame
                                               :frame-manager (find-frame-manager :port port)
                                               :width 800 :height 600))
           (enable-frame frame)
           (funcall function socket (find-pane-named frame 'lines)))
      (when frame (ignore-errors (destroy-frame frame)))
      (when port (release-mock-port port))
      (release-mock-socket socket))))

(define-application-frame typing-frame ()
  ()
  (:panes (io :interactor :scroll-bars nil))
  (:layouts (default io)))

(defun typed-into-interactor (text)
  "Queue a key press for each character of TEXT and then Return on an
interactor, ACCEPT a string from it on the calling thread, and return a
plist of the :VALUE read, the :MESSAGES sent, and the :BLITS among them."
  (let* ((socket (make-mock-socket))
         (port nil)
         (frame nil))
    (unwind-protect
         (progn
           (setf port (make-mock-port socket :width 800 :height 600)
                 frame (make-application-frame 'typing-frame
                                               :frame-manager (find-frame-manager :port port)
                                               :width 800 :height 600))
           (enable-frame frame)
           (stop-port-event-loop port)
           (let ((pane (find-pane-named frame 'io)))
             (flet ((press (char name)
                      (queue-event pane (make-instance 'key-press-event
                                                       :sheet pane
                                                       :key-name name
                                                       :key-character char
                                                       :modifier-state 0))))
               (loop for char across text
                     do (press char (intern (string char) :keyword)))
               (press #\Return :return))
             (clear-sent-scripts socket)
             (let* ((value (let ((*application-frame* frame))
                             (accept 'string :stream pane :prompt "text")))
                    (scripts (sent-scripts socket)))
               (list :value value
                     :messages (length scripts)
                     :blits (blit-count scripts)))))
      (when frame (ignore-errors (destroy-frame frame)))
      (when port (release-mock-port port))
      (release-mock-socket socket))))

(defun scripts-for (socket pane event)
  "The scripts sent while PANE handles EVENT on the calling thread."
  (clear-sent-scripts socket)
  (let ((*application-frame* (pane-frame pane)))
    (handle-event pane event))
  (sent-scripts socket))

(defun wheel-event (pane)
  "A downward wheel event over PANE."
  (make-instance 'pointer-scroll-event
                 :sheet pane
                 :pointer (climi::port-pointer (port pane))
                 :x 10 :y 10
                 :modifier-state 0
                 :delta-x 0 :delta-y 48
                 :button +pointer-wheel-down+))

(defun keystroke (pane)
  "A key press of the letter a, addressed to PANE."
  (make-instance 'key-press-event
                 :sheet pane
                 :key-name :|a|
                 :key-character #\a
                 :modifier-state 0))

(defun button-press (pane)
  "A left button press over PANE."
  (make-instance 'pointer-button-press-event
                 :sheet pane
                 :pointer (climi::port-pointer (port pane))
                 :x 10 :y 10
                 :modifier-state 0
                 :button +pointer-left-button+))

(defun last-text-position (scripts)
  "The index in SCRIPTS of the last fillText script, or NIL."
  (position-if (lambda (script) (search ".fillText(" script)) scripts
               :from-end t))

(defun first-blit-position (scripts)
  "The index in SCRIPTS of the first drawImage script, or NIL."
  (position-if (lambda (script) (search ".drawImage(" script)) scripts))

(defun presents-finished-frame-p (scripts)
  "True when SCRIPTS draw text and blit only after the last text is drawn,
so no blit presents a cleared or half-drawn pane."
  (let ((blit (first-blit-position scripts))
        (text (last-text-position scripts)))
    (and blit text (> blit text))))

(defun history-size (pane)
  "The number of top-level output records in PANE's output history."
  (let ((count 0))
    (map-over-output-records (lambda (record)
                               (declare (ignore record))
                               (incf count))
                             (stream-output-history pane))
    count))

;;; ------------------------------------------------------------------
;;; One event, one presented frame.
;;; ------------------------------------------------------------------

(define-test one-wheel-event-presents-one-frame
  (call-with-redrawing-pane
   (lambda (socket pane)
     (let ((scripts (scripts-for socket pane (wheel-event pane))))
       (is = 1 (blit-count scripts))
       (true (presents-finished-frame-p scripts))))))

(define-test one-keystroke-presents-one-frame
  (call-with-redrawing-pane
   (lambda (socket pane)
     (let ((scripts (scripts-for socket pane (keystroke pane))))
       (is = 1 (blit-count scripts))
       (true (presents-finished-frame-p scripts))))))

(define-test one-redisplay-presents-one-frame
  (call-with-redrawing-pane
   (lambda (socket pane)
     (clear-sent-scripts socket)
     (let ((frame (pane-frame pane)))
       (let ((*application-frame* frame))
         (redisplay-frame-panes frame :force-p t)))
     (let ((scripts (sent-scripts socket)))
       (is = 1 (blit-count scripts))
       (true (presents-finished-frame-p scripts))))))

(define-test repeated-redisplays-keep-one-history
  (call-with-redrawing-pane
   (lambda (socket pane)
     (declare (ignore socket))
     (let ((frame (pane-frame pane))
           (counts '()))
       (let ((*application-frame* frame))
         (dotimes (i 3)
           (redisplay-frame-pane frame pane :force-p t)
           (push (history-size pane) counts)))
       (is equal (list *line-count* *line-count* *line-count*)
           (reverse counts))))))

(define-test typing-into-an-interactor-presents-once-per-gesture
  ;; The input editor echoes from STREAM-READ-GESTURE, after HANDLE-EVENT
  ;; has returned.  Expect one frame as each gesture is read, which shows
  ;; the prompt or the previous echo, and one as the editor exits.
  (let* ((text "abcde")
         (result (typed-into-interactor text)))
    (is equal text (getf result :value))
    (is = (+ (length text) 2) (getf result :blits))))

;;; ------------------------------------------------------------------
;;; Drawing outside an event still reaches the screen.
;;; ------------------------------------------------------------------

(define-test drawing-outside-an-event-presents-at-once
  (call-with-redrawing-pane
   (lambda (socket pane)
     (clear-sent-scripts socket)
     (format pane "progress~%")
     (finish-output pane)
     (true (plusp (blit-count (sent-scripts socket)))))))

(define-test waiting-for-input-presents-pending-drawing
  (call-with-redrawing-pane
   (lambda (socket pane)
     (let* ((*blits-before-wait* nil)
            (scripts (scripts-for socket pane (button-press pane))))
       (is eql 1 *blits-before-wait*)
       (is = 1 (blit-count scripts))))))
