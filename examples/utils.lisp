
;; Some examples of the misc macros and utility functions in PAL


;; Get path to applications user specific data directory. Application name is taken from the :title argument
;; to OPEN/WITH-PAL so be careful to set it to something sensible.
;; If the directory doesn't exists it is created, the exact location of the files is OS dependant.

(pal:get-application-folder)
(pal:get-application-file "saved_game.data")


;; DO-N is like DO-TIMES but it iterates over the cartesian product of its arguments. Handy when working with tilemaps etc.

(pal:do-n (i 3 j 3 k 3)
  (format t "~a ~a ~a~%" i j k))


;; RANDOMLY evaluates its body, umm, randomly.

(pal:randomly 10
  (print "I'm a lucky s-expression!")) ;; has a 1/10 chance to get evaluated


;; CURRY, your average currying macro

(mapcar (pal:curry '* 2 2) '(1 2 3 4 5))


;; RANDOM-ELT returns a random element in a sequence

(pal:random-elt (mapcar (pal:curry '* 2 2) '(1 2 3 4 5)))


;; CLAMPs a value between min and max

(pal:clamp 10 (random 30) 20)


;; DATA-PATH searches for a file from the PATHS given to PAL and returns the first match

(pal:data-path "foo.png")


;; LOAD-FOREIGN-LIBRARIES loads all the required dynamic libraries. Normally you don't need to use this
;; but if you are distributing apps with CLisp you need to call this in your init-fn before any other PAL functions.

(pal:load-foreign-libraries)