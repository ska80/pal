(in-package :pal)

(declaim (optimize (speed 3)
                   (safety 1)))


(defstruct color
  (r 0 :type u8)
  (g 0 :type u8)
  (b 0 :type u8)
  (a 0 :type u8))


(declaim (inline color))
(defun color (r g b &optional (a 255))
  (declare (type u8 r) (type u8 g) (type u8 b) (type u8 a))
  (make-color :r r :g g :b b :a a))


(defun random-color ()
  (color (random 255) (random 255) (random 255) (random 255)))