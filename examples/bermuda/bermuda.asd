
(in-package #:asdf)

(defsystem bermuda
  :components
  ((:file "bermuda" :depends-on ("sprites" "resources" "particles" "package"))
   (:file "sprites" :depends-on ("resources" "package"))
   (:file "resources" :depends-on ("package"))
   (:file "particles" :depends-on ("package"))
   (:file "package"))
  :depends-on ("pal"))


