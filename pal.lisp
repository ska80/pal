(declaim (optimize (speed 3)
                   (safety 3)))

(in-package :pal)

(defparameter *pal-directory* (make-pathname :directory (pathname-directory *load-pathname*)))
(defvar *messages* nil)
(defvar *pal-running* nil)
(defvar *title* "")
(defvar *ticks* 0)
(defvar *clip-stack* nil)
(defvar *fps* 0)
(defvar *new-fps* 0)
(defvar *delay* 0)
(defvar *max-fps* 0)
(defvar *data-paths* nil)
(defvar *pressed-keys* nil)
(defvar *width* 0)
(defvar *height* 0)
(defvar *cursor* nil)
(defvar *cursor-offset* (v 0 0))
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *current-image* nil)

(declaim (type list *messages*)
         (type list *clip-stack*)
         (type hash-table *pressed-keys*)
         (type u16 *mouse-x*)
         (type u16 *mouse-y*)
         (type u16 *width*)
         (type u16 *height*)
         (type vec *cursor-offset*)
         (type integer *ticks*)
         (type fixnum *new-fps*)
         (type fixnum *fps*)
         (type u11 *max-fps*)
         (type u11 *delay*)
         (type (or boolean image) *cursor*)
         (type (or boolean image) *current-image*))


(defgeneric open-pal (&key width height fps title fullscreenp paths))
(defmethod open-pal (&key (width 800) (height 600) (fps 60) (title "PAL") (fullscreenp nil) (paths nil))
  (when *pal-running*
    (close-pal))
  (pal-ffi:init (logior pal-ffi:+init-video+ pal-ffi:+init-audio+))
  (pal-ffi:open-audio 22050 pal-ffi:+audio-s16+ 2 1024) ;; 4096
  (pal-ffi:gl-set-attribute pal-ffi:+gl-depth-size+ 0)
  (pal-ffi:gl-set-attribute pal-ffi:+gl-doublebuffer+ 1)
  (when (cffi:null-pointer-p (pal-ffi::set-video-mode
                              width
                              height
                              0
                              (logior (if fullscreenp
                                          pal-ffi::+fullscreen+
                                          0)
                                      pal-ffi:+opengl+)))
    (error "PAL failed to obtain SDL surface"))
  (pal-ffi:set-caption title (cffi:null-pointer))
  (pal-ffi:gl-disable pal-ffi:+gl-cull-face-test+)
  (pal-ffi:gl-enable pal-ffi:+gl-texture-2d+)
  (pal-ffi:gl-shade-model pal-ffi:+gl-flat+)
  (pal-ffi:gl-disable pal-ffi:+gl-scissor-test+)
  (set-blend-mode :blend)
  (pal-ffi:gl-viewport 0 0 width height)
  (pal-ffi:gl-matrix-mode pal-ffi:+gl-projection+)
  (pal-ffi:gl-load-identity)
  (pal-ffi:gl-ortho 0d0 (coerce width 'double-float) (coerce height 'double-float) 0d0 -1d0 1d0)
  (pal-ffi:gl-matrix-mode pal-ffi:+gl-modelview+)
  (pal-ffi:gl-load-identity)
  (pal-ffi:gl-alpha-func pal-ffi:+gl-greater+ 0.0f0)
  (clear-screen 0 0 0)
  (reset-tags)
  (setf *data-paths* nil
        *messages* nil
        *pressed-keys* (make-hash-table :test 'eq)
        *ticks* (get-internal-real-time)
        *title* title
        *current-image* nil
        *max-fps* (truncate 1000 fps)
        *ticks* (pal-ffi:get-tick)
        *clip-stack* nil
        *fps* 1
        *delay* 0
        *new-fps* 0
        *cursor* t
        *cursor-offset* (v 0 0)
        *width* width
        *height* height
        *pal-running* t)
  (add-path *default-pathname-defaults*)
  (add-path *pal-directory*)
  (if (listp paths)
      (dolist (p paths)
        (add-path p))
      (add-path paths)))

(declaim (inline clamp))
(defun clamp (min v max)
  (max min (min max v)))

(defun relt (sequence)
  (elt sequence (random (length sequence))))

(defun free-all-resources ()
  (reset-tags)
  (pal-ffi:halt-music)
  (pal-ffi:halt-channel -1)
  (unless (eq *cursor* t)
    (set-cursor nil))
  (pal-ffi:free-all-resources))

(defgeneric close-pal ())
(defmethod close-pal ()
  (unwind-protect
       (progn (free-all-resources)
              (pal-ffi:close-audio)
              (pal-ffi:show-cursor t))
    (pal-ffi:quit)
    (setf *pal-running* nil)))

(defun get-application-folder ()
  (assert (> (length *title* ) 0))
  #-win32 (ensure-directories-exist (merge-pathnames (make-pathname :directory (list :relative (concatenate 'string "." *title*)))
                                                     (user-homedir-pathname)))
  #+win32 (ensure-directories-exist (merge-pathnames (make-pathname :directory (list :relative *title*))
                                                     (parse-namestring (pal-ffi:get-application-folder)))))

(defun get-application-file (file)
  (merge-pathnames file (get-application-folder)))

(defun add-path (path)
  (if #-:clisp (probe-file path)
      #+:clisp (ext:probe-directory path)
      (pushnew path *data-paths*)
      (warn "Illegal data path: ~a" path)))

(defun data-path (file)
  (let ((result nil))
    (dolist (i *data-paths* result)
      (when (probe-file (merge-pathnames file i))
        (setf result (namestring (merge-pathnames file i)))))
    (if result
        result
        (error "Data file not found: ~a" file))))

(defun get-info ()
  (format nil "Vendor: ~a~%Renderer: ~a~%Version: ~a~%Extensions: ~a~%"
          (pal-ffi:gl-get-string pal-ffi:+gl-vendor+)
          (pal-ffi:gl-get-string pal-ffi:+gl-renderer+)
          (pal-ffi:gl-get-string pal-ffi:+gl-version+)
          (pal-ffi:gl-get-string pal-ffi:+gl-extensions+)))



;; Events

(declaim (inline key-pressed-p))
(defun key-pressed-p (keysym)
  (gethash keysym *pressed-keys*))

(defun keysym-char (keysym)
  (code-char (cffi:foreign-enum-value 'pal-ffi:sdl-key keysym)))

(declaim (inline get-mouse-pos))
(defun get-mouse-pos ()
  (v *mouse-x* *mouse-y*))

(declaim (inline get-mouse-x))
(defun get-mouse-x ()
  *mouse-x*)

(declaim (inline get-mouse-y))
(defun get-mouse-y ()
  *mouse-y*)

(defun dispatch-event (&key key-up-fn key-down-fn mouse-motion-fn mouse-button-up-fn mouse-button-down-fn quit-fn)
  (block event-loop
    (cffi:with-foreign-object (event :char 100)
      (do-event event key-up-fn key-down-fn mouse-motion-fn mouse-button-up-fn mouse-button-down-fn quit-fn))))

(defun wait-keypress ()
  (let ((key nil))
    (event-loop
        (:key-down-fn (lambda (k)
                        (setf key k)
                        (return-from event-loop key))))
    (event-loop
        (:key-up-fn (lambda (k)
                      (when (eq key k)
                        (return-from event-loop key)))))
    key))



;; Screen

(declaim (inline update-screen))
(defun update-screen ()
  (let ((e (pal-ffi:gl-get-error)))
    (unless (= e 0)
      (error "GL error ~a" e)))
  (setf *new-fps* (max 1 (the fixnum (- (pal-ffi:get-tick) *ticks*))))
  (setf *ticks* (pal-ffi:get-tick))
  (setf *fps* (truncate (+ *fps* *new-fps*) 2))
  (if (> *delay* 1)
      (decf *delay*))
  (when (< *fps* *max-fps*)
    (incf *delay* 2))
  (pal-ffi:delay *delay*)
  (if (or (eq t *cursor*) (eq nil *cursor*))
      nil
      (with-transformation ()
        (with-blend (:mode :blend :r 255 :g 255 :b 255 :a 255)
          (pal-ffi:gl-load-identity)
          (draw-image *cursor* (v- (get-mouse-pos) *cursor-offset*))
          (let ((y 0)
                (fh (get-font-height)))
            (declare (type u11 y fh))
            (dolist (m *messages*)
              (declare (type simple-string m))
              (draw-text m (v 0 (incf y fh))))))))
  (pal-ffi:gl-swap-buffers))

(declaim (inline get-screen-width))
(defun get-screen-width ()
  *width*)

(declaim (inline get-screen-height))
(defun get-screen-height ()
  *height*)

(declaim (inline get-fps))
(defun get-fps ()
  (truncate 1000 *fps*))

(declaim (inline clear-screen))
(defun clear-screen (r g b)
  (declare (type u8 r g b))
  (pal-ffi:gl-clear-color (/ r 255f0) (/ g 255f0) (/ b 255f0) 255f0)
  (pal-ffi:gl-clear pal-ffi:+gl-color-buffer-bit+))

(defun set-mouse-pos (x y)
  (pal-ffi:warp-mouse x y)
  (setf *mouse-x* x
        *mouse-y* y))

(defun set-cursor (image &optional offset)
  (declare (type (or image boolean) image))
  (assert (or (image-p image) (typep image 'boolean)))
  (when offset
    (setf *cursor-offset* offset))
  (cond
    ((eq image t)
     (pal-ffi:show-cursor t))
    ((eq image nil)
     (pal-ffi:show-cursor nil))
    ((image-p image)
     (setf *cursor* image)
     (pal-ffi:show-cursor nil)))
  image)

(defun push-clip (x y width height)
  (pal-ffi:gl-scissor x y width height)
  (pal-ffi:gl-enable pal-ffi:+gl-scissor-test+)
  (push (vector x y width height) *clip-stack*))

(defun pop-clip ()
  (pop *clip-stack*)
  (if *clip-stack*
      (let ((r (first *clip-stack*)))
        (pal-ffi:gl-scissor (aref r 0) (aref r 1) (aref r 2) (aref r 3)))
      (pal-ffi:gl-disable pal-ffi:+gl-scissor-test+)))




;; State

(declaim (inline set-blend-mode))
(defun set-blend-mode (mode)
  (case mode
    ((nil) (pal-ffi:gl-disable pal-ffi:+gl-blend+))
    (:blend (pal-ffi:gl-enable pal-ffi:+gl-blend+)
            (pal-ffi:gl-blendfunc pal-ffi:+gl-src-alpha+ pal-ffi:+gl-one-minus-src-alpha+))
    (:additive (pal-ffi:gl-enable pal-ffi:+gl-blend+)
               (pal-ffi:gl-blendfunc pal-ffi:+gl-src-alpha+ pal-ffi:+gl-one+))))

(declaim (inline rotate))
(defun rotate (angle)
  (declare (type single-float angle))
  (pal-ffi:gl-rotatef angle 0f0 0f0 1f0))

(declaim (inline scale))
(defun scale (x y)
  (declare (type single-float x y))
  (pal-ffi:gl-scalef x y 1f0))

(declaim (inline translate))
(defun translate (vec)
  (declare (type vec vec))
  (pal-ffi:gl-translatef (vx vec) (vy vec) 0f0))

(declaim (inline reset-blend-mode))
(defun reset-blend-mode ()
  (set-blend-mode :blend)
  (set-blend-color 255 255 255 255))

(declaim (inline set-blend-color))
(defun set-blend-color (r g b a)
  (declare (type u8 r g b a))
  (pal-ffi:gl-color4ub r g b a))

(declaim (inline set-image))
(defun set-image (image)
  (declare (type image image))
  (unless (eq image *current-image*)
    (setf *current-image* image)
    (pal-ffi:gl-bind-texture pal-ffi:+gl-texture-2d+ (pal-ffi::image-texture image))))




;; Images

(defun surface-get-pixel (image x y)
  (declare (type u11 x y))
  (let* ((bpp (cffi:foreign-slot-value (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pixelformat) 'pal-ffi:pixelformat 'pal-ffi:bytesperpixel))
         (pixels (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pixels))
         (pitch (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pitch))
         (point (+ (* y pitch) (* x bpp)))
         (pixel (case bpp
                  (1 (cffi:mem-ref pixels :uint8 point))
                  (2 (cffi:mem-ref pixels :uint16 point))
                  (3 (logior (logior (logior (cffi:mem-ref pixels :uint8 point)
                                             (ash (cffi:mem-ref pixels :uint8 (+ point 1)) 8)))
                             (ash (cffi:mem-ref pixels :uint8 (+ point 2)) 16)))
                  (4 (cffi:mem-ref pixels :uint32 point))
                  (otherwise (error "Unhandled bpp in surface-get-pixel")))))
    (cffi:with-foreign-objects ((r :uint8)
                                (g :uint8)
                                (b :uint8)
                                (a :uint8))
      (pal-ffi:get-rgba pixel (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pixelformat) r g b a)
      (values (cffi:mem-ref r :uint8)
              (cffi:mem-ref g :uint8)
              (cffi:mem-ref b :uint8)
              (cffi:mem-ref a :uint8)))))



(defun make-texture-from-surface (surface smooth-p)
  (let* ((width (min 1024 (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:w)))
         (height (min 1024 (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:h)))
         (texture-width (expt 2 (or (find-if (lambda (x)
                                               (> (expt 2 x)
                                                  (1- width)))
                                             '(6 7 8 9 10)) 10)))
         (texture-height (expt 2 (or (find-if (lambda (x)
                                                (> (expt 2 x)
                                                   (1- height)))
                                              '(6 7 8 9 10)) 10)))
         (id (cffi:foreign-alloc :uint :count 1))
         (tdata (cffi:foreign-alloc :uint64 :count (/ (* texture-width texture-height) 2) :initial-element 0)))
    (do-n (x width y height)
      (multiple-value-bind (r g b a) (surface-get-pixel surface x y)
        (let ((p (the fixnum (+ (* y (the u16 (* (the u11 texture-width) 4))) (the u16 (* 4 x))))))
          (setf (cffi:mem-ref tdata :uint8 p) (the u8 r)
                (cffi:mem-ref tdata :uint8 (+ p 1)) (the u8 g)
                (cffi:mem-ref tdata :uint8 (+ p 2)) (the u8 b)
                (cffi:mem-ref tdata :uint8 (+ p 3)) (the u8 a)))))
    (pal-ffi:gl-gen-textures 1 id)
    (pal-ffi:gl-bind-texture pal-ffi:+gl-texture-2d+ (cffi:mem-ref id :uint))
    (pal-ffi:gl-tex-parameteri pal-ffi:+gl-texture-2d+ pal-ffi:+gl-texture-mag-filter+ (if smooth-p pal-ffi:+gl-linear+ pal-ffi:+gl-nearest+))
    (pal-ffi:gl-tex-parameteri pal-ffi:+gl-texture-2d+ pal-ffi:+gl-texture-min-filter+ (if smooth-p pal-ffi:+gl-linear+ pal-ffi:+gl-nearest+))
    (pal-ffi:gl-teximage2d pal-ffi:+gl-texture-2d+
                           0
                           (if (= (cffi:foreign-slot-value (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:pixelformat)
                                                           'pal-ffi:pixelformat 'pal-ffi:bytesperpixel)
                                  3)
                               pal-ffi:+gl-rgb+
                               pal-ffi:+gl-rgba+)
                           texture-width texture-height 0 pal-ffi:+gl-rgba+ pal-ffi:+gl-unsigned-byte+ tdata)
    (cffi:foreign-free tdata)
    (let ((image (pal-ffi::make-image :texture (cffi:mem-ref id :uint)
                                      :tx2 (coerce (/ width texture-width) 'single-float)
                                      :ty2 (coerce (/ height texture-height) 'single-float)
                                      :texture-width texture-width
                                      :texture-height texture-height
                                      :width (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:w)
                                      :height (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:h))))
      (setf *current-image* image)
      (pal-ffi:register-resource image))))

(defun load-image (file &optional (smooth-p nil))
  (let* ((surface (pal-ffi:load-image (data-path file)))
         (image (make-texture-from-surface surface smooth-p)))
    (pal-ffi::free-surface surface)
    image))

(defun draw-image (image pos &optional angle scale)
  (declare (type image image) (type vec pos) (type (or boolean single-float) angle scale))
  (set-image image)
  (let ((width (image-width image))
        (height (image-height image))
        (tx2 (pal-ffi:image-tx2 image))
        (ty2 (pal-ffi:image-ty2 image)))
    (if angle
        (with-transformation ()
          (translate pos)
          (rotate angle)
          (when scale
            (scale scale scale))
          (let ((x (- (/ (image-width image) 2f0)))
                (y (- (/ (image-height image) 2f0))))
            (with-gl pal-ffi:+gl-quads+
              (pal-ffi:gl-tex-coord2f 0f0 0f0)
              (pal-ffi:gl-vertex2f x y)
              (pal-ffi:gl-tex-coord2f tx2 0f0)
              (pal-ffi:gl-vertex2f (+ x width) y)
              (pal-ffi:gl-tex-coord2f tx2 ty2)
              (pal-ffi:gl-vertex2f (+ x width) (+ y height))
              (pal-ffi:gl-tex-coord2f 0f0 ty2)
              (pal-ffi:gl-vertex2f x (+ y height)))))
        (with-gl pal-ffi:+gl-quads+
          (pal-ffi:gl-tex-coord2f 0f0 0f0)
          (pal-ffi:gl-vertex2f (vx pos) (vy pos))
          (pal-ffi:gl-tex-coord2f tx2 0f0)
          (pal-ffi:gl-vertex2f (+ (vx pos) width) (vy pos))
          (pal-ffi:gl-tex-coord2f tx2 ty2)
          (pal-ffi:gl-vertex2f (+ (vx pos) width) (+ (vy pos) height))
          (pal-ffi:gl-tex-coord2f 0f0 ty2)
          (pal-ffi:gl-vertex2f (vx pos) (+ (vy pos) height))))))

(defun draw-quad (image a b c d)
  (declare (type image image) (type vec a b c d))
  (set-image image)
  (let ((tx2 (pal-ffi:image-tx2 image))
        (ty2 (pal-ffi:image-ty2 image)))
    (with-gl pal-ffi:+gl-quads+
      (pal-ffi:gl-tex-coord2f 0f0 0f0)
      (pal-ffi:gl-vertex2f (vx a) (vy a))
      (pal-ffi:gl-tex-coord2f tx2 0f0)
      (pal-ffi:gl-vertex2f (vx b) (vy b))
      (pal-ffi:gl-tex-coord2f tx2 ty2)
      (pal-ffi:gl-vertex2f (vx c) (vy c))
      (pal-ffi:gl-tex-coord2f 0f0 ty2)
      (pal-ffi:gl-vertex2f (vx d) (vy d)))))

(defun draw-image-from (image from-pos to-pos width height)
  (declare (type image image) (type vec from-pos to-pos) (type u11 width height))
  (set-image image)
  (let* ((vx (vx from-pos))
         (vy (vy from-pos))
         (vx-to (vx to-pos))
         (vy-to (vy to-pos))
         (tx1 (/ vx (pal-ffi:image-texture-width image)))
         (ty1 (/ vy (pal-ffi:image-texture-height image)))
         (tx2 (/ (+ vx width) (pal-ffi:image-texture-width image)))
         (ty2 (/ (+ vy height) (pal-ffi:image-texture-height image))))
    (with-gl pal-ffi:+gl-quads+
      (pal-ffi:gl-tex-coord2f tx1 ty1)
      (pal-ffi:gl-vertex2f vx-to vy-to)
      (pal-ffi:gl-tex-coord2f tx2 ty1)
      (pal-ffi:gl-vertex2f (+ vx-to width) vy-to)
      (pal-ffi:gl-tex-coord2f tx2 ty2)
      (pal-ffi:gl-vertex2f (+ vx-to width) (+ vy-to height))
      (pal-ffi:gl-tex-coord2f tx1 ty2)
      (pal-ffi:gl-vertex2f vx-to (+ vy-to height)))))

(declaim (inline draw-line))
(defun draw-line (la lb r g b a &optional (width 1.0f0))
  (declare (type vec la lb) (type u8 r g b a) (type single-float width))
  (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
  (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
  (set-blend-color r g b a)
  (pal-ffi:gl-line-width width)
  (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
  (with-gl pal-ffi:+gl-lines+
    (pal-ffi:gl-vertex2f (vx la) (vy la))
    (pal-ffi:gl-vertex2f (vx lb) (vy lb)))
  (pal-ffi:gl-pop-attrib))


(declaim (inline draw-arrow))
(defun draw-arrow (la lb r g b a &optional (width 1.0f0))
  (declare (type vec la lb) (type u8 r g b a) (type single-float width))
  (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
  (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
  (set-blend-color r g b a)
  (pal-ffi:gl-line-width width)
  (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
  (let ((d (v* (v-direction la lb) (+ width 8f0))))
    (with-gl pal-ffi:+gl-lines+
      (pal-ffi:gl-vertex2f (vx la) (vy la))
      (pal-ffi:gl-vertex2f (vx lb) (vy lb))
      (pal-ffi:gl-vertex2f (vx lb) (vy lb))
      (pal-ffi:gl-vertex2f (vx (v+ lb (v-rotate d 140f0)))
                           (vy (v+ lb (v-rotate d 140f0))))
      (pal-ffi:gl-vertex2f (vx lb) (vy lb))
      (pal-ffi:gl-vertex2f (vx (v+ lb (v-rotate d -140f0)))
                           (vy (v+ lb (v-rotate d -140f0))))))
  (pal-ffi:gl-pop-attrib))



(declaim (inline draw-point))
(defun draw-point (pos r g b a &optional (size 1f0))
  (declare (type vec pos) (type u8 r g b a) (type single-float size))
  (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
  (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
  (pal-ffi:gl-enable pal-ffi:+gl-point-smooth+)
  (pal-ffi:gl-point-size size)
  (set-blend-color r g b a)
  (with-gl pal-ffi:+gl-point+
    (pal-ffi:gl-vertex2f (vx pos) (vy pos)))
  (pal-ffi:gl-pop-attrib))

(defun draw-rectangle (pos width height r g b a &optional (filledp t))
  (declare (type vec pos) (type u11 width height) (type u8 r g b a) (type boolean filledp))
  (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
  (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
  (set-blend-color r g b a)
  (cond
    (filledp
     (pal-ffi:gl-rectf (vx pos) (vy pos) (+ (vx pos) width) (+ (vy pos) height)))
    (t
     (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
     (with-gl pal-ffi:+gl-line-loop+
       (pal-ffi:gl-vertex2f (vx pos) (vy pos))
       (pal-ffi:gl-vertex2f (+ (vx pos) width) (vy pos))
       (pal-ffi:gl-vertex2f (+ (vx pos) width) (vy pos))
       (pal-ffi:gl-vertex2f (+ (vx pos) width) (+ (vy pos) height))
       (pal-ffi:gl-vertex2f (+ (vx pos) width) (+ (vy pos) height))
       (pal-ffi:gl-vertex2f (vx pos) (+ (vy pos) height))
       (pal-ffi:gl-vertex2f (vx pos) (+ (vy pos) height)))))
  (pal-ffi:gl-pop-attrib))

(defun draw-polygon (points r g b a &optional (fill t) image)
  (declare (type list points) (type u8 r g b a) (type symbol fill) (type (or image boolean) image))
  (cond
    ((and (eq fill t) image)
     (set-image image)
     (with-gl pal-ffi:+gl-polygon+
       (let ((dx (vx (first points)))
             (dy (vy (first points))))
         (dolist (p points)
           (let* ((x (vx p))
                  (y (vy p))
                  (tx (/ (- x dx) (pal-ffi:image-texture-width image)))
                  (ty (/ (- y dy) (pal-ffi:image-texture-height image))))
             (pal-ffi:gl-tex-coord2f tx ty)
             (pal-ffi:gl-vertex2f x y))))))
    ((and (listp fill) image)
     (set-image image)
     )
    ((eq nil fill)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
     (set-blend-color r g b a)
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
     (with-gl pal-ffi:+gl-line-loop+
       (dolist (p points)
         (pal-ffi:gl-vertex2f (vx p) (vy p))))
     (pal-ffi:gl-pop-attrib))
    ((eq t fill)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
     (set-blend-color r g b a)
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (with-gl pal-ffi:+gl-polygon+
       (dolist (p points)
         (pal-ffi:gl-vertex2f (vx p) (vy p))))
     (pal-ffi:gl-pop-attrib))))




;;; Samples

(defun load-sample (file &optional (volume 128))
  "Volume 0-128"
  (let ((sample (pal-ffi:load-wav (data-path file))))
    (pal-ffi:volume-chunk (pal-ffi:sample-chunk sample) volume)
    sample))

(declaim (inline play-sample))
(defun play-sample (sample &optional (loops 0) (angle 0) (distance 0))
  "Angle is an integer between 0-360. Distance is an integer between 0-255."
  (let ((channel (pal-ffi:play-channel -1 (pal-ffi:sample-chunk sample) loops)))
    (pal-ffi:set-position channel angle distance)
    channel))

(defun set-sample-volume (sample volume)
  "Volume 0-128."
  (pal-ffi:volume-chunk (pal-ffi:sample-chunk sample) volume))




;;; Music

(defun load-music (file)
  (pal-ffi:load-music (data-path file)))

(defun play-music (music &optional (loops -1) (volume 128))
  "Volume 0-128, -1 for loops is repeat"
  (pal-ffi:volume-music volume)
  (pal-ffi:play-music (pal-ffi:music-music music) loops))

(defun set-music-volume (volume)
  "Volume 0-128"
  (pal-ffi:volume-music volume))

(defun halt-music ()
  (pal-ffi:halt-music))




;; Fonts

(defstruct glyph
  (char #\space :type character)
  (x 0 :type u11)
  (y 0 :type u11)
  (width 0 :type u11)
  (height 0 :type u11)
  (xoff 0 :type fixnum)
  (dl 0 :type u11))


(defun load-font (font)
  (let ((glyphs (make-array 255 :initial-element (make-glyph :x 0 :y 0 :width 1 :height 1 :xoff 0 :dl 0) :element-type 'glyph))
        (lines (with-open-file (file (data-path (concatenate 'string font ".fnt")))
                 (loop repeat 4 do (read-line file))
                 (loop for i from 0 to 94 collecting
                       (substitute #\space #\, (subseq (read-line file) 6) :start 1)))))
    (dolist (line lines)
      (let ((glyph (glyph-from-line line)))
        (setf (aref glyphs (char-code (glyph-char glyph)))
              glyph)))
    (pal-ffi:register-resource (pal-ffi:make-font :image (load-image (concatenate 'string font ".png"))
                                                  :height (glyph-height (aref glyphs 32))
                                                  :glyphs glyphs))))

(defun glyph-from-line (line)
  (let ((char (elt line 0))
        (coords (read-from-string (concatenate 'string "(" (subseq line 2) ")"))))
    (make-glyph :char char
                :dl 0
                :x (first coords)
                :y (second coords)
                :width (third coords)
                :height (fourth coords)
                :xoff (sixth coords))))

(defun draw-glyph (char font)
  (declare (type font font) (type character char))
  (let ((image (pal-ffi:font-image font))
        (g (aref (pal-ffi:font-glyphs font) (char-code char))))
    (draw-image-from image
                     (v (glyph-x g)
                        (glyph-y g))
                     (v 0 0)
                     (glyph-width g)
                     (glyph-height g))
    (pal-ffi:gl-translatef (coerce (+ (glyph-width g) (glyph-xoff g)) 'single-float) 0f0 0f0)))

(defun draw-text (text pos &optional font)
  (declare (type vec pos) (type simple-string text) (type (or font boolean) font))
  (with-transformation (:pos pos)
    (let ((font (if font
                    font
                    (tag 'default-font))))
      (loop for c across text do
            (draw-glyph c font)))))

(declaim (inline get-font-height))
(defun get-font-height (&optional font)
  (declare (type (or font boolean) font))
  (pal-ffi:font-height (if font
                           font
                           (tag 'default-font))))

(defun get-text-size (text &optional font)
  (declare (type (or font boolean) font) (type simple-string text))
  (values (let ((glyphs (pal-ffi:font-glyphs (if font
                                                 font
                                                 (tag 'default-font)))))
            (loop for c across text summing
                  (+ (glyph-width (aref glyphs (char-code c)))
                     (glyph-xoff (aref glyphs (char-code c))))))
          (pal-ffi:font-height (if font
                                   font
                                   (tag 'default-font)))))

(defun draw-fps ()
  (draw-text (prin1-to-string (get-fps)) (v 0 0)))

(defun message (object)
  (setf *messages* (append *messages* (list (prin1-to-string object))))
  (when (> (length *messages*) (- (truncate (get-screen-height) (get-font-height)) 2))
    (pop *messages*)))