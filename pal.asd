
(in-package #:asdf)

(defsystem pal
  :description "Pixel Art Library"
  :author "Tomi Neste"
  :license "MIT"
  :components
  ((:file "ffi"
          :depends-on ("package"))
   (:file "vector"
          :depends-on ("package"))
   (:file "pal-macros"
          :depends-on ("ffi" "vector"))
   (:file "pal"
          :depends-on ("pal-macros"))
   (:file "package"))
  :depends-on ("cffi"))


