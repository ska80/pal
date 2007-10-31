;; NOTE: the following example is intentionally slow and somewhat obfuscated


(defun swarm ()
  (let ((vectors nil))
    (pal:with-pal (:width 1024 :height 768)
      (pal:event-loop (:key-down-fn (lambda (key)
                                      (pal:message key)
                                      (when (eq key :key-mouse-1)
                                        (setf vectors (append vectors (loop repeat 50 collecting (cons (pal:get-mouse-pos)
                                                                                                       (pal:v-random 5.0))))))))
        (pal:draw-rectangle (pal:v 0 0) 1024 768 0 0 0 128)
        (pal:with-blend (:color (pal:color 255 128 128))
          (pal:draw-text "Use left mousekey to add particles." (pal:v 0 0)))

        (let ((midpoint (pal:v/ (reduce 'pal:v+ vectors :initial-value (pal:v 0 0) :key 'car)
                                (max 1
                                     (coerce (length vectors) 'single-float)))))
          (pal:draw-point midpoint 255 0 0 255 :size 10 :smoothp t)
          (setf vectors (mapcar (lambda (v)
                                  (cons (pal:v+ (car v) (cdr v))
                                        (pal:v* (pal:v+ (cdr v)
                                                        (pal:v+ (pal:v/ (pal:v-direction midpoint (car v))
                                                                        (max 1
                                                                             (sqrt (pal:v-distance midpoint (car v)))))
                                                                (pal:v-direction (car v) (pal:get-mouse-pos))
                                                                ))
                                                .9)))
                                vectors)))
        (pal:with-blend (:mode :additive)
          (dolist (v vectors)
            (pal:draw-arrow (car v)
                            (pal:v+ (car v) (cdr v))
                            10 7 0 255
                            :size 10)))))))

;; (swarm)