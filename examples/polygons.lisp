(defpackage poly-tests
  (:use :cl :pal))
(in-package :poly-tests)


(with-pal ()
  (let ((grid (load-image "bg2.png"))
        (slad (load-image "save_lisp.gif"))
        (teddy (load-image "yellow-teddy.png")))
    (event-loop ()

      ;; DRAW-RECTANGLE just draws a filled or wireframe rectangle on screen

      (draw-rectangle (v 0 0)
                      800 600
                      0 0 0 32 :filledp t)

      ;; DRAW-POLYGON draw a polygon which vertexes are given as a list of VECs.
      ;; FILL is either nil, true or image that is used as a pattern. If fill is an image the rgba values are not used.
      ;; When ABSOLUTEP is T image patterns position is decided by screen coordinates.


      (with-transformation (:pos (v 100 100))
        (draw-polygon (list (v -100 0)
                            (v 100 0)
                            (v 50 100)
                            (v -50 100)
                            )
                      255 0 0 255
                      :fill grid
                      :absolutep t)
        (draw-polygon (list (v -100 0)
                            (v 100 0)
                            (v 50 100)
                            (v -50 100)
                            )
                      255 0 0 255
                      :fill nil :size 5f0
                      :absolutep nil))

      ;; Note: next one doesn't work like you might expect since the image size is rounded up
      ;; to the nearest power of two and the extra is filled with blank.

      (with-blend (:color '(255 255 255 20))
        (draw-polygon (list (v+ (get-mouse-pos) (v -100 -100))
                            (v+ (get-mouse-pos) (v 100 -100))
                            (v+ (get-mouse-pos) (v 100 100))
                            (v+ (get-mouse-pos) (v -100 100)))
                      0 0 0 0
                      :absolutep t
                      :fill slad))


      ;; DRAW-IMAGE-FROM draws a part of image, defined by a starting point, width and height.
      ;; If width or height are larger than the source image the image is tiled

      (draw-image-from teddy (v 0 (get-mouse-y))
                       (v (get-mouse-x) 0)
                       (truncate (image-width teddy) 2)
                       (get-screen-height))
      (draw-image-from teddy (v (truncate (image-width teddy) 2) (get-mouse-y) )
                       (v (- (get-screen-width) (get-mouse-x)) 0)
                       (truncate (image-width teddy) 2)
                       (get-screen-height))

      ;; (draw-quad ...) to be done

      )))