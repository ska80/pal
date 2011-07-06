;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-user)

(defpackage #:net.common-lisp.pal-test
  (:use #:cl #:net.common-lisp.pal))

(in-package #:net.common-lisp.pal-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(eos:def-suite eos:in-suite eos:run! eos:signals
            eos:is eos:is-false eos:is-true eos:is-every))
  (export 'run-all-tests))

(defmacro test (name &body body)
  `(eos:test ,name ,@body))

(def-suite pal)

(defun run-all-tests ()
  (run! 'pal))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :net.common-lisp.pal-test))))
  (run-all-tests))
