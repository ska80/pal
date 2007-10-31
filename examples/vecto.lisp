;;; Few utility functions and examples of using PAL with Vecto (http://www.xach.com/lisp/vecto/)

(defpackage :pal-vecto
  (:use :cl)
  (:export state-to-image))
(in-package :pal-vecto)



(defun state-to-image (smoothp)
  "Convert current Vecto graphics state to PAL image."
  (let* ((data (vecto::image-data vecto::*graphics-state*))
	 (width (vecto::width vecto::*graphics-state*))
	 (height (vecto::height vecto::*graphics-state*))
	 (image (make-array (list width height))))
    (pal:do-n (x width y height)
      (let ((pixel (+ (* x 4) (* y width 4))))
	(setf (aref image x y)
	      (list (aref data pixel)
		    (aref data (+ pixel 1))
		    (aref data (+ pixel 2))
		    (aref data (+ pixel 3))))))
    (pal:image-from-array image smoothp)))
