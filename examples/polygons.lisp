(defpackage poly-tests
  (:use :cl :pal))
(in-package :poly-tests)


(with-pal (:paths (merge-pathnames "examples/" pal::*pal-directory*))
  (let ((grid (load-image "bg2.png"))
        (plane (load-image "lego-plane.png" t))
        (slad (load-image "save_lisp.gif"))
        (teddy (load-image "yellow-teddy.png")))
    (event-loop ()

      (draw-rectangle (v 0 0)
                      800 600
                      0 0 0 32) ;; Draw a black, transparent rectangle over the scene.
      ;; (clear-screen 0 0 0) ;; Use this instead if the afterimages give you a headache.

      ;; DRAW-IMAGE draw the whole image at given position. Keyword arguments can be given to define the
      ;; scale, angle and horizontal and vertical alignment ("hotspot")

      (draw-image plane
                  (v 700 500)
                  :halign :middle ;; Possible options are :left, :right and :middle. :left is the default.
                  :valign :bottom ;; -''- :top, :bottom, :middle. :top is the default.
                  :angle (v-angle (v-direction (v 700 500) (get-mouse-pos))) ;; angle in degrees
                  :scale (* (v-distance (v 700 500) (get-mouse-pos)) .01f0))

      (draw-point (v 700 500) 255 0 0 255 :size 10f0) ;; Draw a red point at the hotspot of previous image.

      ;; DRAW-POLYGON draw a polygon which vertexes are given as a list of VECs.
      ;; FILL is either nil, t or image that is used as a pattern. If fill is an image the rgba values have no effect.
      ;; When ABSOLUTEP is T image patterns position is decided by screen coordinates.
      ;; Max value of SIZE depends on the OpenGL implementation, you probably shouldn't use values greater than 10f0

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
                      :fill nil :size 4f0
                      :absolutep nil))



      ;; DRAW-RECTANGLEs arguments are similar to DRAW-POLYGON
      ;; Notice how the size of the actual SLAD image used is expanded up to the nearest power of two and the extra space is filled with blank,
      ;; usually this happens transparently to the user (eg. image-width returns the original width of image) but in some cases
      ;; it can cause some artifacts. In this case if the original image had width and height of power of two it would be seamlessly
      ;; tiled across the screen.
      ;; For example, image of size 65x30 will be expanded to the size 128x32, so it is a
      ;; good idea to try and fit the image sizes inside the nearest power of two to save memory.

      (with-blend (:color '(255 255 255 128))
        (draw-rectangle (get-mouse-pos)
                        100 100
                        0 0 0 0
                        :absolutep t
                        :fill slad))


      ;; DRAW-IMAGE* draws a part of image, defined by a starting point, width and height.
      ;; If width or height are larger than the source image the image is tiled
      ;; Like with DRAW-POLYGON non-power-of-two image sizes can give unexpected results.

      (let ((x (abs (- 400 (get-mouse-x)))))
        (draw-image* teddy
                     (v 0 (get-mouse-y))
                     (v x 0)
                     (truncate (image-width teddy) 2)
                     (get-screen-height))
        (draw-image* teddy
                     (v (truncate (image-width teddy) 2) (get-mouse-y) )
                     (v (- (get-screen-width) x) 0)
                     (truncate (image-width teddy) 2)
                     (get-screen-height))))))