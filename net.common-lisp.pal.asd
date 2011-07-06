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

(in-package #:cl-user)

#+pal-debug
(progn
  (pushnew :pal-trace *features*)
  (declaim (optimize debug (speed 1))))
#-pal-debug
(declaim (optimize speed (safety 1)))

(defpackage #:net.common-lisp.pal-asd
  (:use #:cl))

(in-package #:net.common-lisp.pal-asd)

(asdf:defsystem :net.common-lisp.pal
  :version "1.1.0"
  :description "Pixel Art Library."
  :author "Tomi Neste"
  :license "MIT"
  :depends-on (:cffi)
  :components
  ((:module :src
    :components
    ((:file "packages")
     (:file "ffi"     :depends-on ("packages"))
     (:file "color"   :depends-on ("ffi"))
     (:file "macros"  :depends-on ("ffi" "color"))
     (:file "vector"  :depends-on ("macros"))
     (:file "pal"     :depends-on ("color" "vector"))))))

(asdf:defsystem :net.common-lisp.pal-test
  :description "Unit tests for APL."
  :depends-on (:net.common-lisp.pal :eos)
  :components
  ((:module :test
    :components
    ((:file "tests")
     (:file "pal"  :depends-on ("tests"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :net.common-lisp.pal))))
  (asdf:load-system :net.common-lisp.pal-test)
  (asdf:test-system :net.common-lisp.pal-test))
