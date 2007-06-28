
(in-package #:asdf)

(defsystem pal
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


