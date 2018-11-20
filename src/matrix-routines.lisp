(in-package :funcl)

(defvar *determinant* 
  (make-instance 'funcl-function 
                 :domain 'square-matrix 
                 :range 'scalar
                 :lambda-function (lambda (matrix) (magicl:det matrix))
                 :differentiator ()))

(defvar *trace*
  (make-instance 'funcl-function
                 :domain 'square-matrix 
                 :range 'scalar 
                 :lambda-function (lambda (matrix)
                                    (reduce #'+ (loop for i from 0 below (magicl:matrix-cols matrix) 
                                                   collecting (magicl:ref matrix i i))))))

(defvar *inverse* 
  (make-instance 'funcl-function 
                 :domain 'square-matrix 
                 :range 'square-matrix
                 :lambda-function (lambda (matrix) (magicl:inv matrix))))
(defvar *identity*
  (make-instance 'funcl-function 
                 :domain 'square-matrix
                 :range 'square-matrix 
                 :lambda-function #'identity))
