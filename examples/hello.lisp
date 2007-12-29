
;;; Few very simple example programs.


(defun hello-1 ()
  (pal:with-pal (:title "Hello!" :paths (merge-pathnames "examples/" pal::*pal-directory*))
    (print (pal:get-gl-info))
    (let ((font (pal:load-font "georgia")))
      (loop for y from 0 to 300 by 2 do
           (pal:draw-line (pal:v 0 (* y 2)) (pal:v 800 (* y 2))
                          50 50 255 (truncate y 2) :smoothp t))
      (let ((midpoint (pal:v-round
                       (pal:v (/ (- (pal:get-screen-width)
                                    (pal:get-text-size "Hello from PAL" font))
                                 2)
                              (/ (- (pal:get-screen-height)
                                    (pal:get-font-height font))
                                 2)))))
        (pal:set-blend-color (pal:color 0 0 0 255))
        (pal:draw-text "Hello from PAL" (pal:v+ midpoint (pal:v 5 5)) font)
        (pal:reset-blend)
        (pal:draw-text "Hello from PAL" midpoint font)))
    (pal:wait-keypress)))

;; (hello-1)


(defun hello-2 ()
  (pal:with-pal (:fps 10000)
    (let ((angle 0f0))
      (pal:set-blend-color (pal:color 0 255 0 255))
      (pal:event-loop ()
        (pal:draw-rectangle (pal:v 0 0)
                            (pal:get-screen-width) (pal:get-screen-height)
                            0 0 0 10)
        (pal:with-transformation (:pos (pal:v 400 300) :angle (incf angle 1) :scale 3)
          (pal:draw-text "Hello from PAL" (pal:v 0 0)))))))

;; (hello-2)


(defun hello-3 ()
  (pal:with-pal (:fps 10000)
    (pal:event-loop ()
      (pal:clear-screen pal:+black+)
      (loop for x from 0 to 800 by (pal:get-text-size "Hello from PAL")
         do
         (loop for y from 20 to 600 by 10
            do
            (pal:draw-text "Hello from PAL" (pal:v x y))))
      (pal:draw-fps))))

;; (hello-3)