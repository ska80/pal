(defpackage :image-tests
  (:use :cl :pal))
(in-package :image-tests)


(define-tags
  ;; IMAGE-FROM-FN builds and image by calling the FN with x and y coordinates.
  ;; FN should return at least three VALUES for r, g and b and an optional fourth value for alpha.
  image-1 (image-from-fn 256 256 nil
                         (lambda (x y)
                           (values (truncate (+ 127 (* 128 (sin (/ x 10)))))
                                   (truncate (+ 127 (* 128 (cos (/ y 10)))))
                                   (truncate (+ 127 (* 128 (cos (/ (+ x y) 10)))))
                                   (truncate (+ 127 (* 128 (cos (/ (- x y) 10))))))))
  ;; IMAGE-FROM-ARRAY builds and image from an 2d array of (list r g b &optional a)
  ;; Try setting the SMOOTHP parameter to T and see what happens.
  image-2 (image-from-array nil #2A(((255 255 255 128) (0 0 0) (255 255 255))
                                    ((255 255 255) (255 255 0) (255 255 255))
                                    ((255 255 255) (0 0 0) (255 255 255 128)))))



(with-pal ()
  (set-cursor (tag 'image-2)) ;; sets image-2 as a mouse cursor image
  (let ((a 0f0))
    (event-loop ()
      (draw-polygon* (list (v 0 0)
                           (v 800 0)
                           (v 800 600)
                           (v 0 600))
                     :colors (list (list 255 0 0 255)
                                   (list 255 0 0 255)
                                   (list 0 0 255 255)
                                   (list 0 0 255 255))) ;; just draws a nice gradient background

      ;; And draw a pattern of image-1s on the top of it. Not exactly seamlessly tiled but hey...
      (draw-rectangle (v 0 0) 800 600 255 255 255 255 :fill (tag 'image-1))

      ;; let's scale up a bit to see what the image-2 looks like.
      (with-transformation (:pos (v 400 300) :scale a)
        (draw-image (tag 'image-2)
                    (v 0 0)
                    :valign :middle
                    :halign :middle
                    :angle (incf a .5f0))))))