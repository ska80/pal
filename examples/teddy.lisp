(defpackage :pal-example
  (:use :cl :pal))
(in-package :pal-example)


(defparameter *sprites* nil)            ; our sprites
(defparameter *blend-mode* :blend)      ; current blend-mode to use
(defparameter *y-scroll* 0)

;; Lets define some resources, this is not strictly necessary but it makes thing simpler.
;; we aren't using any sfx this time, see the *-SAMPLE & *-MUSIC functions for those.


(define-tags plane (load-image "lego-plane.png" t) ; loads the image with smooth interpolation, nicer for rotated images
             teddy (load-image "yellow-teddy.png" t)
             tile (load-image "ground.png")
             cursor (load-image "cursor.png")
             engine (load-sample "Flight.wav")

             ;; djbierman-hrrrr.mp3 is licensed under
             ;; Creative Commons License Deed
             ;; Attribution 2.0
             ;; You are free:
             ;; to Share -- to copy, distribute, display, and perform the work
             ;; to Remix -- to make derivative works
             ;; Under the following conditions:
             ;; Attribution. You must attribute the work in the manner specified by the author or licensor.
             ;; For any reuse or distribution, you must make clear to others the license terms of this work.
             ;; Any of these conditions can be waived if you get permission from the copyright holder.
             music (load-music "djbierman+hrrrr.ogg"))


;; main classes

(defclass sprite ()
  ((pos :accessor pos-of :initarg :pos :initform (v 0 0))
   (vel :accessor vel-of :initarg :vel :initform (v 0 0))
   (alt :accessor alt-of :initarg :alt :initform 10)
   (image :accessor image-of :initarg :image)
   (angle :accessor angle-of :initarg :angle :initform 0)))

(defmethod initialize-instance :after ((sprite sprite) &key &allow-other-keys)
  (push sprite *sprites*))

(defmethod act ((s sprite))
  (v+! (pos-of s) (vel-of s))
  (v*! (vel-of s) .98)
  (v*! (vel-of s) .98))

(defmethod draw ((s sprite))
  (draw-image (image-of s)
              (pos-of s)
              :valign :middle
              :halign :middle
              :angle (angle-of s)))




(defclass plane (sprite)
  ()
  (:default-initargs :image (tag 'plane)))

(defmethod act ((s plane))
  (v+! (vel-of s)
       (v* (v-direction (pos-of s) (get-mouse-pos)) .3))
  (setf (angle-of s) (v-angle (vel-of s)))
  (call-next-method))



(defclass mutant-teddy (sprite)
  ()
  (:default-initargs :image (tag 'teddy)))

(defmethod act ((s mutant-teddy))
  (setf (angle-of s) (mod (+ (angle-of s) 1) 360))
  (call-next-method))



(defun example ()
  (with-pal (:fullscreenp nil :width 800 :height 600 :fps 60 :paths (merge-pathnames "examples/" pal::*pal-directory*))
    ;; inits PAL, the args used are the default values.
    ;; PATHS is a pathname or list of pathnames that PAL uses to find the resource files loaded with LOAD-* functions.
    ;; By default PATHS contains the PAL source directory and value of *default-pathname-defaults*
    ;; only call PAL functions (with the expection of DEFINE-TAGS forms) inside WITH-PAL or between OPEN-PAL and CLOSE-PAL

    (setf *sprites* nil)

    ;; Hide the mouse cursor and use cursor.png instead. 18,18 is the offset ("hotspot") for the cursor image
    ;; Other possible options to cursor are: t - show the default cursor, nil - hide all cursors
    (set-cursor (tag 'cursor) (v 18 18))

    (play-music (tag 'music) :fade 10000)
    (play-sample (tag 'engine) :loops t :volume 50)

    (make-instance 'plane :alt 20)
    (dotimes (i 20)
      (make-instance 'mutant-teddy
                     :pos (v (random (get-screen-width))
                             (random (get-screen-height)))
                     :vel (v-random 3.0)
                     :angle (random 360.0)))

    (event-loop ()
      ;; simple event loop, no mouse-move, key-down etc. handlers defined, we'll handle input explicitly with TEST-KEYS.
      ;; The default key-down handler quits the event-loop when ESC is pressed, if you define your own key-down-handler
      ;; don't forget to make sure there is a way to quit pal (especially when in fullscreen).
      ;; to define e.g. a key-handler use a form like (event-loop (:key-down-fn (lambda (key) ...)) ...)
      ;; you can quit the event loop with (return-from event-loop)

      ;; first, draw a scrolling tiled background
      (draw-image* (tag 'tile)
                   (v 0 0)
                   (v 0 (- *y-scroll* 64))
                   (get-screen-width)
                   (+ (get-screen-height) 64))
      (setf *y-scroll* (mod (+ *y-scroll* 1) 64))

      ;; then the sprites, first the shadows
      ;; sorting the sprites and their shadows according to their altitude is left as an exercise to the reader

      (with-blend (:color (color 0 0 0 128))
        (dolist (i *sprites*)
          (with-transformation (:pos (v (alt-of i) (alt-of i)))
            (draw i))))

      (with-blend (:mode *blend-mode*)
        (dolist (i *sprites*)
          (draw i)

          ;; Let's do this for CLisp or we might a get nasty floating-point-undereflow error in the vector operations.
          #+CLISP (ext:without-floating-point-underflow
                      (act i))
          #-CLISP (act i)))

      ;; TEST-KEYS is used to check if some key is currently pressed, _all_ the matching forms are evaluated.
      (test-keys
        (:key-f (halt-music 10000))
        (:key-1 (setf *blend-mode* nil)
                (message *blend-mode*))
        (:key-2 (setf *blend-mode* :blend)
                (message *blend-mode*))
        ;; We can also test for several keys at once:
        ((:key-3 :key-space :key-mouse-1) (setf *blend-mode* :additive)
         (message *blend-mode*)))

      (draw-fps) ;; Draw the frames/second counter to the top left corner.
      (draw-text "Press key to select blend-mode:" (v 200 (* 0 (get-font-height))))
      (draw-text "1=nil 2=:blend 3=:additive" (v 200 (* 1 (get-font-height))))
      (draw-text "Press F to fade out the music." (v 200 (* 2 (get-font-height)))))))

;; (example)