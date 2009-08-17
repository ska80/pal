(in-package :bermuda)

(declaim (optimize (speed 3)
                   (safety 0)
                   (debug 1)))

(defparameter *particles* nil)
(declaim (list *particles*))

(defstruct particle
  (pos (v 0 0) :type vec)
  (vel (v 0 0) :type vec)
  image
  (age 0 :type u11)
  (scale 1f0 :type single-float))



(defun particle (image pos &optional (dir (v 0 0)) (scale 1.0f0))
  (let ((p (make-particle :image image :pos pos :vel (v+ (v* (v-random 0.7) (+ (random 1.0) .01)) dir) :age 255 :scale scale)))
    (push p *particles*)))

(defun init-particles ()
  (setf *particles* nil))

(defun explosion (image pos)
  (play-sample (tag 'explosion-1))
  (dotimes (i 5)
    (particle image (copy-vec pos))))

(defun draw-particles ()
  (dolist (p *particles*)
    (declare (type particle p))
    (v+! (particle-pos p)
         (particle-vel p))
    (v*! (particle-vel p) 0.99)
    (decf (particle-age p) 2)
    (when (and (> (particle-age p) 180) (= (random 300) 0))
      (play-sample (tag 'explosion-2))
      (dotimes (i 3)
        (particle (particle-image p)
                  (copy-vec (particle-pos p))
                  (particle-vel p)
                  (* (particle-scale p) .90))))
    (when (<= (particle-age p) 1)
      (setf *particles* (remove p *particles*))))
  (with-blend (:mode :blend)
    (dolist (p *particles*)
      (declare (type particle p))
      (set-blend-color (color 0 0 0 (min 255 (* (particle-age p) 2))))
      (draw-image (particle-image p)
                  (screen-pos (particle-pos p))
                  :angle (* .5 (particle-age p))
                  :scale (particle-scale p))))

  (with-blend (:mode :additive)
    (dolist (p *particles*)
      (declare (type particle p))
      (set-blend-color (color 255 (particle-age p) 30 (particle-age p)))
      (draw-image (particle-image p)
                  (screen-pos (particle-pos p))
                  :angle (* .5 (particle-age p))
                  :scale (particle-scale p)))))