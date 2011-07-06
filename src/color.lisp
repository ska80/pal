;;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;;
;;;; Pixel Art Library is published under the MIT license
;;;;
;;;; Copyright (c) 2006 Tomi Neste
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is furnished to do
;;;; so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(in-package #:net.common-lisp.pal)

(declaim (optimize (speed 3)
                   (safety 1)))


(defstruct color
  (r 0 :type u8)
  (g 0 :type u8)
  (b 0 :type u8)
  (a 0 :type u8))


(declaim (inline color))
(defun color (r g b &optional (a 255))
  (declare (type u8 r) (type u8 g) (type u8 b) (type u8 a))
  (make-color :r r :g g :b b :a a))


(defun random-color ()
  (color (random 255) (random 255) (random 255) (random 255)))




(defparameter +black+ (color 0 0 0))
(defparameter +gray+ (color 128 128 128))
(defparameter +light-gray+ (color 200 200 200))
(defparameter +dark-gray+ (color 64 64 64))
(defparameter +white+ (color 255 255 255))
(defparameter +light-green+ (color 50 255 50))
(defparameter +light-blue+ (color 50 50 255))
(defparameter +red+ (color 255 20 20))
