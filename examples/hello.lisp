
;;; Few very simple example programs.


(defun hello-1 ()
  (pal:with-pal (:paths "/path/to/examples/)
    (let ((font (pal:load-font "georgia")))
      (pal:draw-text "Hello from PAL"
                     (pal:v-round
                      (pal:v (/ (- (pal:get-screen-width)
                                   (pal:get-text-size "Hello from PAL" font))
                                2)
                             (/ (- (pal:get-screen-height)
                                   (pal:get-font-height font))
                                2)))
                     font))
    (pal:wait-keypress)))

;; (hello-1)


(defun hello-2 ()
  (pal:with-pal ()
    (let ((angle 0f0))
      (pal:event-loop ()
        (pal:draw-rectangle (pal:v 0 0)
                            (pal:get-screen-width) (pal:get-screen-height)
                            50 50 200 10)
        (pal:with-transformation (:pos (pal:v 400 300) :angle (incf angle 1f0) :scale 3f0)
          (pal:draw-text "Hello from PAL" (pal:v 0 0)))))))

;; (hello-2)