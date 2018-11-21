(in-package :funcl)

(annot:enable-annot-syntax)

@export
(defvar *determinant* 
  (make-instance 'funcl-function 
                 :domain 'square-matrix 
                 :range 'scalar
                 :lambda-function (lambda (matrix) (magicl:det matrix))
                 :differentiator ()))

@export
(defvar *trace*
  (make-instance 'funcl-function
                 :domain 'square-matrix 
                 :range 'scalar 
                 :lambda-function (lambda (matrix)
                                    (reduce #'+ (loop for i from 0 below (magicl:matrix-cols matrix) 
                                                   collecting (magicl:ref matrix i i))))))

@export
(defvar *inverse* 
  (make-instance 'funcl-function 
                 :domain 'square-matrix 
                 :range 'square-matrix
                 :lambda-function (lambda (matrix) (magicl:inv matrix))))

@export
(defvar *identity*
  (make-instance 'funcl-function 
                 :domain 'square-matrix
                 :range 'square-matrix 
                 :lambda-function #'identity))
