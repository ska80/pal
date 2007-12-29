(defpackage :image-tests
  (:use :cl :pal))
(in-package :image-tests)


(define-tags
  ;; IMAGE-FROM-FN builds an image by calling the FN with x and y coordinates.
  ;; FN should return at least three VALUES for r, g and b and an optional fourth value for alpha.
  image-1 (image-from-fn 255 255 nil
                         (lambda (x y)
                           (values (truncate (+ 127 (* 128 (sin (/ x 10)))))
                                   (truncate (+ 127 (* 128 (cos (/ y 10)))))
                                   (truncate (+ 127 (* 128 (cos (/ (+ x y) 10)))))
                                   (truncate (+ 127 (* 128 (cos (/ (- x y) 10))))))))
  ;; IMAGE-FROM-ARRAY builds an image from a 2d array of (list r g b &optional a)
  ;; Try setting the SMOOTHP parameter to T and see what happens.
  image-2 (image-from-array #2A(((255 255 255 128) (0 0 0) (255 255 255))
                                ((255 255 255) (255 255 0) (255 255 255))
                                ((255 255 255) (0 0 0) (255 255 255 128)))
                            nil)

  ;; LOAD-IMAGE-TO-ARRAY does exactly what it says. Let's load the plane image and randomize the alpha values a bit.
  image-3 (image-from-array (let ((image (load-image-to-array "lego-plane.png")))
                              (do-n (x (array-dimension image 0) y (array-dimension image 1))
                                (when (> (fourth (aref image x y)) 200)
                                  (setf (fourth (aref image x y)) (+ (random 128) 127))))
                              image)
                            nil))



(with-pal (:paths (merge-pathnames "examples/" pal::*pal-directory*))
  (set-cursor (tag 'image-3))
  ;; sets image-3 as a mouse cursor image
  (let ((a 0))
    (event-loop ()
      (draw-polygon* (list (v 0 0)
                           (v 800 0)
                           (v 800 600)
                           (v 0 600))
                     :colors (list (list 255 0 0 255)
                                   (list 255 0 0 255)
                                   (list 0 0 255 255)
                                   (list 0 0 255 255)))
      ;; just draws a nice gradient background

      ;; And draw a pattern of image-1s on the top of it. Not exactly seamless tiles but hey...
      (draw-rectangle (v 0 0) 800 600 255 255 255 255 :fill (tag 'image-1))

      ;; let's scale up a bit to see what the image-2 looks like.
      (with-transformation (:pos (v 400 300) :scale a)
        (draw-image (tag 'image-2)
                    (v 0 0)
                    :valign :middle
                    :halign :middle
                    :angle (incf a .1)))


      ;; Press left mousebutton to capture part of the screen as a new cursor.
      ;; Note that altough the allocated images are released when PAL is closed we really should manually release
      ;; the old cursor image with FREE-RESOURCE if we keep allocating lots of new images.
      (when (key-pressed-p :key-mouse-1)
        (set-cursor (image-from-array
                     (screen-to-array (get-mouse-pos) 128 128)
                     nil))))))