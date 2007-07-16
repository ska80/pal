(defpackage :image-tests
  (:use :cl :pal))
(in-package :image-tests)


(define-tags image-1 (image-from-fn 255 255 t
                                    (lambda (x y)
                                      (values x 0 x y)))
             image-2 (image-from-array nil #2A(((255 255 255 128) (0 0 0) (255 255 255))
                                               ((255 255 255) (255 255 0) (255 255 255))
                                               ((255 255 255) (0 0 0) (255 255 255 128)))))



(with-pal ()
  (set-cursor (tag 'image-1))
  (event-loop ()
    (clear-screen 50 100 255)
    (with-transformation (:scale 100f0)
      (draw-image (tag 'image-2) (v 0 0)))))