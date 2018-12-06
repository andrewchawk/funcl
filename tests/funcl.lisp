(defpackage funcl-test
  (:use :cl
        :funcl
        :prove)
  (:shadowing-import-from :bld-gen
                          + - * / expt
                          sin cos tan
                          atan asin acos
                          sinh cosh tanh
                          asinh acosh atanh
                          log exp sqrt abs
                          min max signum))
(in-package :funcl-test)

;; NOTE: To run this test file, execute `(asdf:test-system :funcl)' in your Lisp.

(plan nil)

(funcl:merge-readtable)

; single variable

(is #q(1 2 3) #q(1 2 3) :test #'polynomial=)
(is (differentiate #q(1 1 1)) #q(1 2) :test #'polynomial=)
(is (differentiate #q(5 4 3 2 1 2))  #q(4 6 6 4 10) :test #'polynomial=)
(is (+ #q( 4 2 1) #q (8 3)) #q(12 5 1) :test #'polynomial=)
(is (- #q(3 2 1 2)) #q(-3 -2 -1 -2) :test #'polynomial=)
(is (* #q(1 3 5 ) #q(2 4 6)) #q (2 10 28 38 30) :test #'polynomial=)
(is (* #q(1/10 3/10 4/5) 7) #q(7/10 21/10 28/5) :test #'polynomial=)

; multivariate

; evaluate
; differentiate
; multiply polynomials
; multiply by scalar
; add
; subtract
; gradient
; gradient-squared
; laplacian


(finalize)



(defvar *2-by-2* (magicl:make-complex-matrix 2 2 (list 1 2 3 4)))
(defvar *3-by-3* (magicl:make-complex-matrix 3 3 (list 1 2 3 4 5 1 2 3 5 )))
(defvar *2-by-3* (magicl:make-complex-matrix 2 3 (list 1 2 3 4 5 6)))
(defvar *5-by-1* (magicl:make-complex-matrix 5 1 (list 1 2 5 1 2)))
(defvar *1-by-1* (magicl:make-complex-matrix 1 1 (list 5)))
