(declaim (optimize (speed 3)
                   (safety 3)))

(in-package :pal)


(defvar *tags* (make-hash-table :test 'eq))


(defmacro define-tags (&body tags)
  `(progn
     ,@(mapcar (lambda (r)
                 `(setf (gethash ',(first r) *tags*)
                        (cons (lambda () ,(second r)) nil)))
               (loop for (a b) on tags by #'cddr collect (list a b)))))

(defun reset-tags ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (setf (cdr v) nil))
           *tags*)
  (define-tags default-font (load-font "default-font")))

(defun tag (name)
  (let ((resource (gethash name *tags*)))
    (if resource
        (if (cdr resource)
            (cdr resource)
            (setf (cdr resource) (funcall (car resource))))
        (error "Named resource ~a not found" name))))

(defmacro with-resource ((resource init-form) &body body)
  `(let ((,resource ,init-form))
     (prog1 (progn
              ,@body)
       (free-resource ,resource))))


(defmacro with-default-settings (&body body)
  `(with-transformation ()
     (with-blend (:mode :blend :r 255 :g 255 :b 255 :a 255)
       (pal-ffi:gl-load-identity)
       ,@body)))

(defmacro with-blend ((&key (mode t) r g b a) &body body)
  `(progn
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
     ,(unless (eq mode t)
              `(set-blend-mode ,mode))
     ,(when (and r g b a)
            `(set-blend-color ,r ,g ,b ,a))
     ,@body
     (pal-ffi:gl-pop-attrib)))

(defmacro with-clipping ((x y width height) &body body)
  `(progn
     (push-clip ,x ,y ,width ,height)
     ,@body
     (pop-clip)))

(defmacro with-transformation ((&key pos angle scale) &body body)
  `(progn
     (pal-ffi:gl-push-matrix)
     ,(when pos
            `(translate ,pos))
     ,(when angle
            `(pal-ffi:gl-rotatef ,angle 0f0 0f0 1f0))
     ,(when scale
            (let ((s (gensym)))
              `(let ((,s ,scale))
                 (pal-ffi:gl-scalef ,s ,s 1f0))))
     ,@body
     (pal-ffi:gl-pop-matrix)))

(defmacro with-gl (mode &body body)
  `(progn
     (pal-ffi:gl-begin ,mode)
     ,@body
     (pal-ffi:gl-end)))

(defmacro randomly (p &body body)
  `(when (= (random ,p) 0)
     ,@body))

(defmacro do-n ((&rest args) &body body)
  (labels ((expand (args)
             (cond
               ((null args) `(progn ,@body))
               (t `(dotimes ,(list (first args) (second args))
                     (declare (type fixnum ,(first args)))
                     ,(expand (cddr args)))))))
    (expand args)))

(defmacro curry (fn &rest args)
  (let ((rest (gensym)))
    `(lambda (&rest ,rest)
       (declare (dynamic-extent ,rest))
       (apply ,fn ,@args ,rest))))

(defmacro test-keys (&body args)
  `(progn
     ,@(mapcar (lambda (arg)
                 `(when ,(if (listp (first arg))
                             `(or ,@(mapcar (lambda (a)
                                              (list 'key-pressed-p a))
                                            (first arg)))
                             `(key-pressed-p ,(first arg)))
                    ,@(rest arg)))
               args)))

(defmacro funcall? (fn &rest args)
  `(when ,fn
     (funcall ,fn ,@args)))

(defmacro do-event (event key-up-fn key-down-fn mouse-motion-fn quit-fn)
  `(loop while (pal-ffi:poll-event ,event)
      do
      (let ((type (cffi:mem-ref ,event :uint8)))
        (cond

          ((= type pal-ffi:+key-up-event+)
           (let ((keysym (cffi:foreign-slot-value ,event 'pal-ffi:keyboard-event 'pal-ffi:keysym)))
             (setf (gethash (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym))
                            *pressed-keys*)
                   nil)
             (funcall? ,key-up-fn
                       (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym)))))

          ((= type pal-ffi:+key-down-event+)
           (let ((keysym (cffi:foreign-slot-value ,event 'pal-ffi:keyboard-event 'pal-ffi:keysym)))
             (setf (gethash (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym))
                            *pressed-keys*)
                   t)
             (if ,key-down-fn
                 (funcall ,key-down-fn
                          (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym)))
                 (when (eq (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym)) :key-escape)
                   (return-from event-loop)))))

          ((= type pal-ffi:+mouse-motion-event+)
           (setf *mouse-x* (cffi:foreign-slot-value ,event 'pal-ffi:mouse-motion-event 'pal-ffi:x)
                 *mouse-y* (cffi:foreign-slot-value ,event 'pal-ffi:mouse-motion-event 'pal-ffi:y))
           (funcall? ,mouse-motion-fn *mouse-x* *mouse-y*))

          ((= type pal-ffi:+mouse-button-up-event+)
           (let* ((button (cffi:foreign-slot-value ,event 'pal-ffi:mouse-button-event 'pal-ffi:button))
                  (keysym (read-from-string (format nil ":key-mouse-~a" button))))
             (setf (gethash keysym
                            *pressed-keys*) nil)
             (funcall? ,key-up-fn keysym)))

          ((= type pal-ffi:+mouse-button-down-event+)
           (let* ((button (cffi:foreign-slot-value ,event 'pal-ffi:mouse-button-event 'pal-ffi:button))
                 (keysym (read-from-string (format nil ":key-mouse-~a" button))))
             (setf (gethash keysym
                            *pressed-keys*) t)
             (funcall? ,key-down-fn keysym)))

          ((= type pal-ffi:+quit-event+)
           (if ,quit-fn
               (funcall ,quit-fn)
               (return-from event-loop))
           )))))


(defmacro event-loop ((&key key-up-fn key-down-fn mouse-motion-fn quit-fn) &body redraw)
  (let ((event (gensym)))
    `(block event-loop
       (cffi:with-foreign-object (,event :char 1000)
         (loop
            (do-event ,event ,key-up-fn ,key-down-fn ,mouse-motion-fn ,quit-fn)
            ,@redraw
            (update-screen))))))


(defmacro with-pal (args &body body)
  `(progn
     (apply 'open-pal (list ,@args))
     (unwind-protect
          (progn ,@body)
       (close-pal))))


(defmacro with-foreign-vector ((chunk n size) &body body)
  `(let ((,chunk (pal-ffi:calloc ,n ,size)))
     (unwind-protect
          ,@body
       (pal-ffi:free ,chunk))))
