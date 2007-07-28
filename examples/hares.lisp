;;; Graphics and idea shamelessly ripped from Haaf's Game Engines (http://hge.relishgames.com/) 'Thousands of Hares' demo.

(defpackage :pal-example
  (:use :cl :pal))
(in-package :pal-example)


(defparameter *sprites* nil)
(defparameter *blend-mode* :blend)


(define-tags hare (load-image "zazaka.png" t)
             bg (load-image "bg2.png"))


(defclass sprite ()
  ((pos :accessor pos-of :initarg :pos :initform (v 0 0))
   (vel :accessor vel-of :initarg :vel :initform (v 0 0))
   (image :accessor image-of :initarg :image)
   (angle :accessor angle-of :initarg :angle :initform 0)
   (r :accessor r-of :initarg :r)
   (g :accessor g-of :initarg :g)
   (b :accessor b-of :initarg :b)
   (scale :accessor scale-of :initform 1 :initarg :scale)
   (scaled :accessor scaled-of :initarg :scaled)))

(defmethod initialize-instance :after ((sprite sprite) &key &allow-other-keys)
  (push sprite *sprites*))

(defmethod draw ((s sprite))
  (set-blend-color (r-of s) (g-of s) (b-of s) 255)
  (draw-image (image-of s)
              (pos-of s)
              :halign :middle
              :valign :middle
              :angle (angle-of s)
              :scale (scale-of s)))

(defmethod act ((s sprite))
  (setf (angle-of s) (mod (+ (angle-of s) 1) 360))
  (when (or (< (vx (pos-of s)) 0) (> (vx (pos-of s)) (get-screen-width)))
    (setf (vel-of s) (v (- (vx (vel-of s)))
                        (vy (vel-of s)))))
  (when (or (< (vy (pos-of s)) 0) (> (vy (pos-of s)) (get-screen-height)))
    (setf (vel-of s) (v (vx (vel-of s))
                        (- (vy (vel-of s))))))
  (when (or (> (scale-of s) 2) (< (scale-of s) 1/2))
    (setf (scaled-of s) (- (scaled-of s))))
  (incf (scale-of s) (scaled-of s))
  (v+! (pos-of s) (vel-of s)))




(defun example ()
  (with-pal (:width 800 :height 600 :fullscreenp nil :fps 6000 :paths (merge-pathnames "examples/" pal::*pal-directory*))
    (setf *sprites* nil)
    (set-cursor nil)
    (dotimes (i 500)
      (make-instance 'sprite
                     :image (tag 'hare)
                     :scaled (- (random .2) .1)
                     :scale (+ (random 1.5) .5)
                     :r (random 255)
                     :g (random 255)
                     :b (random 255)
                     :pos (v (random (get-screen-width))
                             (random (get-screen-height)))
                     :vel (v-random 3.0)
                     :angle (random 360.0)))

    (event-loop ()

      (draw-rectangle (v 0 0) 800 600 255 255 255 255 :fill (tag 'bg))
      (with-blend (:mode *blend-mode*)
        (dolist (i *sprites*)
          (draw i)
          (act i)))

      (test-keys
        (:key-1 (setf *blend-mode* nil))
        (:key-2 (setf *blend-mode* :blend))
        (:key-3 (setf *blend-mode* :additive)))

      (draw-fps))))

;; (example)