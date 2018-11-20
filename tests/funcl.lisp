(defpackage funcl-test
  (:use :cl
        :funcl
        :prove))
(in-package :funcl-test)

;; NOTE: To run this test file, execute `(asdf:test-system :funcl)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)



(defvar *2-by-2* (magicl:make-complex-matrix 2 2 (list 1 2 3 4)))
(defvar *3-by-3* (magicl:make-complex-matrix 3 3 (list 1 2 3 4 5 1 2 3 5 )))
(defvar *2-by-3* (magicl:make-complex-matrix 2 3 (list 1 2 3 4 5 6)))
(defvar *5-by-1* (magicl:make-complex-matrix 5 1 (list 1 2 5 1 2)))
(defvar *1-by-1* (magicl:make-complex-matrix 1 1 (list 5)))
