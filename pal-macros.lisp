(declaim (optimize (speed 3)
                   (safety 1)))

(in-package :pal)


(defvar *tags* (make-hash-table :test 'eq))

(defmacro define-tags (&body tags)
  `(progn
     ,@(mapcar (lambda (r)
                 `(add-tag ',(first r) (lambda () ,(second r))))
               (loop for (a b) on tags by #'cddr collect (list a b)))))


(defun add-tag (tag fn)
  (assert (and (symbolp tag) (functionp fn)))
  (setf (gethash tag *tags*)
        (cons fn nil)))

(defun reset-tags (&key resource)
  (maphash (if resource
               (lambda (k v)
                 (declare (ignore k))
                 (when (eq resource (cdr v))
                   (setf (cdr v) nil)))
               (lambda (k v)
                 (declare (ignore k))
                 (setf (cdr v) nil)))
           *tags*))

(defun tag (name)
  (declare (type symbol name))
  (let ((resource (gethash name *tags*)))
    (if resource
        (if (cdr resource)
            (the resource (cdr resource))
            (let ((r (funcall (car resource))))
              (assert (resource-p r))
              (the resource (setf (cdr resource) r))))
        (error "Named resource ~a not found" name))))

(defun make-coerce-form (to-type value)
  `(,value ,(case to-type
                  ((u8 u11 u16 integer fixnum) `(truncate ,value))
                  (component `(coerce ,value 'component))
                  (single-float `(coerce ,value 'single-float))
                  (double-float `(coerce ,value 'double-float))
                  (float `(coerce ,value 'float)))))


(defmacro defunct (name lambda-list declarations &body body)
  (let* ((decls (loop for (a b) on declarations by #'cddr collecting
                     `(type ,a ,b)))
         (coerced (remove-if (lambda (decl)
                               (null (second decl)))
                             (mapcar (lambda (decl)
                                       (make-coerce-form (second decl) (third decl)))
                                     decls))))
    (if coerced
        `(defun ,name ,lambda-list
           (let (,@coerced)
             (declare ,@decls)
             ,@body))
        `(defun ,name ,lambda-list
           (declare ,@decls)
           ,@body))))

;; (declaim (ftype (function (double-float double-float) double-float) sss))

(defmacro with-resource ((resource init-form) &body body)
  `(let ((,resource ,init-form))
     (prog1 (progn
              ,@body)
       (free-resource ,resource))))


(defmacro with-default-settings (&body body)
  `(with-transformation ()
     (with-blend (:mode :blend :color '(255 255 255 255))
       (pal-ffi:gl-load-identity)
       ,@body)))


(defmacro with-blend ((&key (mode t) color) &body body)
  `(progn
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
     ,(unless (eq mode t)
              `(set-blend-mode ,mode))
     ,(when color
            `(set-blend-color (first ,color) (second ,color) (third ,color) (fourth ,color)))
     (prog1 (progn
              ,@body)
       (close-quads)
       (pal-ffi:gl-pop-attrib))))

(defmacro with-clipping ((x y width height) &body body)
  `(progn
     (push-clip ,x ,y ,width ,height)
     (prog1 (progn
              ,@body)
       (pop-clip))))

(defmacro with-transformation ((&key pos angle scale) &body body)
  `(progn
     (close-quads)
     (pal-ffi:gl-push-matrix)
     ,(when pos
            `(translate ,pos))
     ,(when angle
            `(rotate ,angle))
     ,(when scale
            (let ((s (gensym)))
              `(let ((,s ,scale))
                 (scale ,s ,s))))
     (prog1 (progn
              ,@body)
       (close-quads)
       (pal-ffi:gl-pop-matrix))))

(defmacro with-gl (mode &body body)
  (if (eq mode 'pal-ffi:+gl-quads+)
      `(progn
         (open-quads)
         ,@body)
      `(progn
         (close-quads)
         (pal-ffi:gl-begin ,mode)
         ,@body
         (pal-ffi:gl-end))))

(defmacro with-line-settings (smoothp size r g b a &body body)
  `(progn
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (set-blend-color ,r ,g ,b ,a)
     (pal-ffi:gl-line-width ,size)
     (if ,smoothp
         (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
         (pal-ffi:gl-disable pal-ffi:+gl-line-smooth+))
     ,@body
     (pal-ffi:gl-pop-attrib)))

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

(declaim (inline funcall?))
(defun funcall? (fn &rest args)
  (declare (type (or function symbol) fn) (dynamic-extent args))
  (if (null fn)
      nil
      (apply fn args)))

(defmacro do-event (event key-up-fn key-down-fn mouse-motion-fn quit-fn)
  `(loop while (pal-ffi:poll-event ,event)
      do
      (let ((type (cffi:mem-ref ,event :uint8)))
        (cond

          ((= type pal-ffi:+key-up-event+)
           (let* ((keysym (cffi:foreign-slot-value ,event 'pal-ffi:keyboard-event 'pal-ffi:keysym))
                  (sym (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym))))
             (setf (gethash sym *pressed-keys*)
                   nil)
             (funcall? ,key-up-fn sym)))

          ((= type pal-ffi:+key-down-event+)
           (let* ((keysym (cffi:foreign-slot-value ,event 'pal-ffi:keyboard-event 'pal-ffi:keysym))
                  (sym (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym))))
             (setf (gethash sym *pressed-keys*)
                   t)
             (if ,key-down-fn
                 (funcall ,key-down-fn sym)
                 (when (eq sym :key-escape)
                   (return-from event-loop)))))

          ((= type pal-ffi:+mouse-motion-event+)
           (setf *mouse-x* (cffi:foreign-slot-value ,event 'pal-ffi:mouse-motion-event 'pal-ffi:x)
                 *mouse-y* (cffi:foreign-slot-value ,event 'pal-ffi:mouse-motion-event 'pal-ffi:y))
           (funcall? ,mouse-motion-fn *mouse-x* *mouse-y*))

          ((= type pal-ffi:+mouse-button-up-event+)
           (let* ((button (cffi:foreign-slot-value ,event 'pal-ffi:mouse-button-event 'pal-ffi:button))
                  (keysym (read-from-string (format nil ":key-mouse-~a" button))))
             (setf (gethash keysym *pressed-keys*)
                   nil)
             (funcall? ,key-up-fn keysym)))

          ((= type pal-ffi:+mouse-button-down-event+)
           (let* ((button (cffi:foreign-slot-value ,event 'pal-ffi:mouse-button-event 'pal-ffi:button))
                  (keysym (read-from-string (format nil ":key-mouse-~a" button))))
             (setf (gethash keysym *pressed-keys*)
                   t)
             (funcall? ,key-down-fn keysym)))

          ((= type pal-ffi:+quit-event+)
           (if ,quit-fn
               (funcall ,quit-fn)
               (return-from event-loop))
           )))))


(defmacro event-loop ((&key key-up-fn key-down-fn mouse-motion-fn quit-fn) &body redraw)
  (let ((event (gensym)))
    `(block event-loop
       (cffi:with-foreign-object (,event :char 500)
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
