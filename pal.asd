
(in-package #:asdf)

(defsystem pal
  :description "Pixel Art Library"
  :author "Tomi Neste"
  :license "MIT"
  :components
  ((:file "ffi"
          :depends-on ("package"))
   (:file "vector"
          :depends-on ("pal-macros"))
   (:file "pal-macros"
          :depends-on ("ffi"))
   (:file "pal"
          :depends-on ("pal-macros" "vector"))
   (:file "package"))
  :depends-on ("cffi"))


