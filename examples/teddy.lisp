(defpackage :pal-example
  (:use :cl :pal))
(in-package :pal-example)


(defparameter *sprites* nil)            ; our sprites
(defparameter *blend-mode* :blend)      ; current blend-mode to use
(defparameter *y-scroll* 0)

;; Lets define some resources, this is not strictly necessary but it makes thing simpler.
;; we aren't using any sfx this time, see the *-SAMPLE & *-MUSIC functions for those.


(define-tags plane (load-image "lego-plane.png" t) ; loads the image with smooth interpolation, nicer for rotated images
             teddy (load-image "yellow-teddy.png" t)
             tile (load-image "ground.png")
             cursor (load-image "cursor.png"))


;; main classes

(defclass sprite ()
  ((pos :accessor pos-of :initarg :pos :initform (v 0 0))
   (vel :accessor vel-of :initarg :vel :initform (v 0 0))
   (image :accessor image-of :initarg :image)
   (angle :accessor angle-of :initarg :angle :initform 0f0)))

(defmethod initialize-instance :after ((sprite sprite) &key &allow-other-keys)
  (push sprite *sprites*))

(defmethod act ((s sprite))
  (v+! (pos-of s) (vel-of s))
  (v*! (vel-of s) .90)
  (v*! (vel-of s) .90))

(defmethod draw ((s sprite))
  (draw-image (image-of s)
              (pos-of s)
              (angle-of s)))




(defclass plane (sprite)
  ()
  (:default-initargs :image (tag 'plane)))

(defmethod act ((s plane))
  (v+! (vel-of s)
       (v* (v-direction (pos-of s) (get-mouse-pos)) .3f0))
  (setf (angle-of s) (v-angle (vel-of s)))
  (call-next-method))



(defclass mutant-teddy (sprite)
  ()
  (:default-initargs :image (tag 'teddy)))

(defmethod act ((s mutant-teddy))
  (setf (angle-of s) (mod (+ (angle-of s) 1f0) 360))
  (call-next-method))



(defun example ()
  (with-pal (:width 800 :height 600 :fullscreenp nil :fps 60)
    ;; inits PAL, the args used are the default values.
    ;; NOTE: fix the PATHS to point to the location of the resource files
    ;; PATHS is a pathname or list of pathnames that defines paths that the LOAD-* functions use for finding resources.
    ;; only call PAL functions (with the expection of DEFINE-TAGS forms) inside WITH-PAL or between OPEN-PAL and CLOSE-PAL

    (setf *sprites* nil)
    (set-cursor (tag 'cursor) (v 18 18))
    (make-instance 'plane)
    (dotimes (i 20)
      (make-instance 'mutant-teddy
                     :pos (v (random (get-screen-width))
                             (random (get-screen-height)))
                     :vel (v-random 3f0)
                     :angle (random 360f0)))

    (event-loop ()
      ;; simple event loop, no mouse-move, key-down etc. handlers defined, we'll handle input explicitly with TEST-KEYS.
      ;; the default key-down handler quits the event-loop when ESC is pressed.
      ;; to define e.g. a key-handler use a form like (event-loop (:key-down-handler (lambda (key) ...)) ...)
      ;; you can quit the event loop with (return-from event-loop)

      ;; first, draw a scrolling tiled background
      (draw-image-from (tag 'tile)
                       (v 0 0)
                       (v 0 (- *y-scroll* 64))
                       (get-screen-width)
                       (+ (get-screen-height) 64))
      (setf *y-scroll* (mod (+ *y-scroll* 1) 64))

      ;; then the sprites
      (with-blend (:mode *blend-mode*)
        (dolist (i *sprites*)
          (draw i)
          #+CLISP (ext:without-floating-point-underflow
                      (act i))
          #-CLISP (act i)))

      (test-keys
        (:key-1 (setf *blend-mode* nil)
                (message *blend-mode*))
        (:key-2 (setf *blend-mode* :blend)
                (message *blend-mode*))
        (:key-3 (setf *blend-mode* :additive)
                (message *blend-mode*)))

      (draw-fps)
      (draw-text "Press key to select blend-mode:" (v 200 (* 0 (get-font-height))))
      (draw-text "1=nil 2=:blend 3=:additive" (v 200 (* 1 (get-font-height)))))))

;; (example)