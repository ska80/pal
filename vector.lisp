(declaim (optimize (speed 3)
                   (safety 3)))

(in-package :pal)

#+CL-HAS-FULL-NUMERIC-TOWER-DAMMIT (deftype component () 'number)
#-CL-HAS-FULL-NUMERIC-TOWER-DAMMIT (deftype component () 'single-float)

(defstruct (vec (:conc-name v))
  (x 0 :type component) (y 0 :type component))

(declaim (inline component))
(defun component (x)
  (coerce x 'component))

(declaim (inline v))
(defun v (x y)
  (make-vec :x (component x) :y (component y)))

(declaim (inline vf))
(defun vf (x y)
  (make-vec :x x :y y))

(declaim (inline rad))
(defun rad (degrees)
  (declare (type component degrees))
  (component (* (/ pi 180) degrees)))

(defun deg (radians)
  (declare (type component radians))
  (component (* (/ 180 pi) radians)))



(defun angle-v (angle)
  (declare (type component angle))
  (v (sin (rad angle)) (- (cos (rad angle)))))

(declaim (inline vec-angle))
(defun v-angle (vec)
  (declare (type vec vec))
  (mod (deg (atan (vx vec)
                  (if (zerop (vy vec))
                      least-negative-short-float
                      (- (vy vec)))))
       360))

(defun v-random (length)
  (v* (angle-v (random 360.0)) length))

(declaim (inline v-round))
(defun v-round (v)
  (declare (type vec v))
  (v (round (vx v)) (round (vy v))))

(declaim (inline v=))
(defun v= (a b)
  (and (= (vx a) (vx b))
       (= (vy a) (vy b))))

(declaim (inline v+!))
(defun v+! (a b)
  (setf (vx a) (+ (vx a) (vx b)))
  (setf (vy a) (+ (vy a) (vy b)))
  nil)

(declaim (inline v+))
(defun v+ (a b)
  (vf (+ (vx a) (vx b))
      (+ (vy a) (vy b))))


(declaim (inline v-))
(defun v- (a b)
  (vf (- (vx a) (vx b))
      (- (vy a) (vy b))))

(declaim (inline v-!))
(defun v-! (a b)
  (setf (vx a) (- (vx a) (vx b)))
  (setf (vy a) (- (vy a) (vy b)))
  nil)


(declaim (inline v*!))
(defun v*! (v m)
  (declare (type component m))
  (setf (vx v) (* (vx v) m))
  (setf (vy v) (* (vy v) m))
  nil)

(declaim (inline v*))
(defun v* (v m)
  (declare (type component m))
  (vf (* (vx v) m)
      (* (vy v) m)))


(declaim (inline v/))
(defun v/ (v d)
  (declare (type component d))
  (vf (/ (vx v) d)
      (/ (vy v) d)))

(declaim (inline v/!))
(defun v/! (v d)
  (declare (type component d))
  (setf (vx v) (/ (vx v) d))
  (setf (vy v) (/ (vy v) d))
  nil)

(declaim (inline v-max))
(defun v-max (a b)
  (if (< (v-magnitude a) (v-magnitude b))
      b a))


(declaim (inline v-min))
(defun v-min (a b)
  (if (< (v-magnitude a) (v-magnitude b))
      a b))


(defun v-rotate (v a)
  (declare (type component a) (type vec v))
  (let ((a (rad a)))
    (vf (- (* (cos a) (vx v))
           (* (sin a) (vy v)))
        (+ (* (sin a) (vx v))
           (* (cos a) (vy v))))))

(declaim (inline v-dot))
(defun v-dot (a b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))))


(declaim (inline v-magnitude))
(defun v-magnitude (v)
  (declare (type vec v))
  (the component (sqrt (the component
                         (+ (expt (vx v) 2)
                            (expt (vy v) 2))))))

(declaim (inline v-normalize))
(defun v-normalize (v)
  (if (/= (v-magnitude v) 0.0)
      (vf (/ (vx v) (v-magnitude v))
          (/ (vy v) (v-magnitude v)))
      (vf 0.0 0.0)))


(defun v-direction (from-vector to-vector)
  (v-normalize (v- to-vector from-vector)))


(declaim (inline v-distance))
(defun v-distance (v1 v2)
  (declare (type vec v1 v2))
  (v-magnitude (v- v1 v2)))

(defun v-truncate (v l)
  (v* (v-normalize v) l))




(defun closest-point-to-line (a b p)
  (declare (type vec a b p))
  (let* ((dir (v- b a))
         (diff (v- p a))
         (len (v-dot dir dir)))
    (if (< len .001)
        nil
        (let ((u (/ (v-dot dir diff) len)))
          (if (> u 0)
              (if (< u 1)
                  (v+ a (v* dir u))
                  b)
              a)))))

(declaim (inline point-in-line))
(defun point-in-line (a b p)
  (declare (type vec a b p))
  (let ((d (v-direction a b)))
    (if (< (abs (+ (v-dot d (v-direction a p))
                   (v-dot d (v-direction b p)))) .00001)
        t nil)))

(defun lines-intersection (la1 la2 lb1 lb2)
  (declare (type vec la1 la2 lb1 lb2))
  (let ((x1 (vx la1))
        (y1 (vy la1))
        (x2 (vx la2))
        (y2 (vy la2))
        (x3 (vx lb1))
        (y3 (vy lb1))
        (x4 (vx lb2))
        (y4 (vy lb2)))
    (let* ((a1 (- y2 y1))
           (b1 (- x1 x2))
           (c1 (- (* x2 y1) (* x1 y2)))
           (a2 (- y4 y3))
           (b2 (- x3 x4))
           (c2 (- (* x4 y3) (* x3 y4)))
           (denom (- (* a1 b2) (* a2 b1))))
      (if (zerop denom)
          nil
          (let ((p (vf (/ (- (* b1 c2) (* b2 c1)) denom)
                       (/ (- (* a2 c1) (* a1 c2)) denom))))
            (if (and (point-in-line la1 la2 p)
                     (point-in-line lb1 lb2 p))
                p
                nil))))))

(defun circle-line-intersection (a b co r)
  (declare (type vec a b co) (type component r))
  (let ((cp (closest-point-to-line a b co)))
    (if cp
        (if (<= (v-distance co cp) r)
            cp
            nil)
        nil)))

(defun distance-from-line (a b p)
  (declare (type vec a b p))
  (let ((cp (closest-point-to-line a b p)))
    (if cp
        (v-distance cp p)
        nil)))

(defun point-inside-rectangle (topleft width height pos)
  (declare (type (or component fixnum) width height) (type vec pos topleft))
  (let* ((x1 (vx topleft))
         (y1 (vy topleft))
         (x2 (+ x1 width))
         (y2 (+ y1 height))
         (x (vx pos))
         (y (vy pos)))
    (if (and (> x x1) (< x x2)
             (> y y1) (< y y2))
        t nil)))