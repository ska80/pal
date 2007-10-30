(in-package :pal)


(defstruct color
  (r 0 :type pal::u8)
  (g 0 :type pal::u8)
  (b 0 :type pal::u8)
  (a 0 :type pal::u8))


(declaim (inline color))
(defun color (r g b a)
  (make-color :r r :g g :b b :a a))


(defun random-color ()
  (color (random 255) (random 255) (random 255) (random 255)))