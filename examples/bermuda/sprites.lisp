(in-package :bermuda)

(defparameter *sprites* nil)
(defparameter *categories* nil)
(defparameter *view* (v 0 0))
(defparameter *shake* 0)
(defparameter *map* nil)
(defconstant +ground-base+ 645)


(defstruct tile
  image)


(defclass sprite ()
  ((pos :accessor pos-of :initarg :pos :type vec)
   (score :accessor score-of :initarg :score :initform 0)
   (hp :accessor hp-of :initarg :hp :initform 0)
   (vel :accessor vel-of :initform (v 0 0) :initarg :vel :type vec)
   (angle :accessor angle-of :initform 0f0 :initarg :angle :type single-float)
   (image :accessor image-of :initarg :image :type image)
   (category :accessor category-of :initarg :category :initform 'sprite :type symbol)))

(declaim (inline screen-pos))
(defun screen-pos (p)
  (declare (type vec p))
  (v-round (v- p *view*)))

(defmethod initialize-instance :after ((sprite sprite) &key &allow-other-keys)
  (let ((c (gethash (category-of sprite) *categories*)))
    (if c
        (push sprite (gethash (category-of sprite) *categories*))
        (setf (gethash (category-of sprite) *categories*) (list sprite))))
  (push sprite *sprites*))

(defmethod hit ((s sprite) dmg)
  (decf (hp-of s) dmg)
  (when (< (hp-of s) 1)
    (kill s)))

(defmethod collidesp ((a sprite) (b sprite))
  (if (< (v-distance (pos-of a) (pos-of b))
         30f0)
      t nil))

(defmethod draw ((s sprite))
  (draw-image (image-of s) (screen-pos (pos-of s))))

(defmethod act ((s sprite))
  (when (or (groundp (pos-of s)))
    (kill s))
  (v+! (pos-of s) (vel-of s)))

(defmethod force ((s sprite) v)
  (v+! (vel-of s) v))

(defmethod kill ((s sprite))
  (incf *score* (score-of s))
  (setf *sprites* (delete s *sprites*)
        (gethash (category-of s) *categories*) (delete s (gethash (category-of s) *categories*))))


(declaim (inline get-sprites))
(defun get-sprites (category)
  (gethash category *categories*))

(defun init-sprites ()
  (setf *sprites* nil
        *view* (v 0 0)
        *categories* (make-hash-table :test 'eq)))

(defun find-sprite (predicate category)
  (find-if predicate
           (get-sprites category)))

(defun alt-at (pos)
  (declare (type vec pos))
  (let ((tile (aref *map* (truncate (vx pos) 256))))
    (- +ground-base+ (image-height (tile-image tile)))))

(declaim (inline groundp))
(defun groundp (pos)
  (declare (type vec pos))
  (> (vy pos) (alt-at pos)))

(defun draw-screen ()
  (with-blend (:mode nil)
    (set-blend-mode nil)
    (draw-image* (tag 'horizon) (v 0 0)
                 (v 0 (* (vy *view*) .1f0 ))
                 800 600))
  (loop for x from (truncate (vx *view*) 256) to (+ (truncate (vx *view*) 256) 5) do
       (let ((tile (aref *map* x)))
         (draw-image (tile-image tile)
                     (screen-pos (v (* x 256)
                                    (- +ground-base+ (image-height (tile-image tile))))))))
  (dolist (s *sprites*)
    (let ((p (- (vx (pos-of s)) (vx *view*))))
      (when (and (> p -100)
                 (< p 900))
        (draw s)
        (act s))))
  (draw-particles))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defclass plane (sprite)
  ())

(defmethod act ((p plane))
  (setf (angle-of p) (- (v-angle (vel-of p)) 90f0))
  (call-next-method))

(defmethod fire ((s plane) bullet-class)
  (make-instance bullet-class
                 :pos (v+ (pos-of s) (v* (vel-of s) 5f0))
                 :vel (v* (angle-v (- (angle-of s) 270f0)) 6f0)))

(defmethod kill ((p plane))
  (setf *shake* 5f0)
  (explosion (tag 'particle) (pos-of p))
  (call-next-method))



(defclass bullet (sprite)
  ((age :accessor age-of :initform 0)
   (dmg :accessor dmg-of :initarg :dmg :initform 10)))

(defmethod act ((b bullet))
  (incf (age-of b))
  (when (> (age-of b) 150)
    (kill b))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass enemy-bullet (bullet)
  ((age :accessor age-of :initform 0))
  (:default-initargs :category 'enemy-bullet :image (tag 'bullet)))


(defclass enemy-plane (plane)
  ()
  (:default-initargs :category 'enemy :image (tag 'ufo) :score 10 :hp 1 :vel (v (- (random 2f0) 1f0) (- (random 1f0) .5f0))))

(defmethod act ((e enemy-plane))
  (setf (angle-of e) (v-angle (vel-of e)))
  (randomly 100
    (fire e 'enemy-bullet))
  (let ((b (find-sprite (lambda (s) (collidesp s e)) 'player-bullet)))
    (when b
      (hit e (dmg-of b))
      (kill b)))
  (call-next-method))

(defmethod draw ((e enemy-plane))
  (draw-image (image-of e)
              (screen-pos (pos-of e))
              :angle (angle-of e)
              :halign :middle :valign :middle))



(defclass player-bullet (bullet)
  ((age :accessor age-of :initform 0))
  (:default-initargs :category 'player-bullet :image (tag 'bullet)))



(defclass player (plane)
  ()
  (:default-initargs :hp 100 :image (tag 'plane)))

(defmethod draw ((s player))
  (draw-image (image-of s)
              (screen-pos (pos-of s))
              :angle (angle-of s)
              :halign :middle :valign :middle))

(defmethod act ((p player))
  (let ((e (find-sprite (lambda (s) (collidesp s p)) 'enemy)))
    (when e
      (hit e 30)
      (hit p 30)))
  (let ((b (find-sprite (lambda (s)
                          (collidesp s p)) 'enemy-bullet)))
    (when b
      (setf *shake* 10f0)
      (hit p (dmg-of b))
      (kill b)))
  (test-keys
    (:key-mouse-1 (play-sample (tag 'shoot))
                  (fire p 'player-bullet)))
  (v*! (vel-of p) .1f0)
  (force p (v* (v-direction (v- (pos-of p) *view*) (get-mouse-pos))
               (* (v-distance (v- (pos-of p) *view*) (get-mouse-pos)) .02f0)))
  (call-next-method))