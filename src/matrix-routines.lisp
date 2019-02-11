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

@export
(defgeneric transpose (matrix))

(defmethod transpose ((matrix magicl:matrix)) (magicl:transpose matrix))
(defmethod transpose ((matrix funcl-function))
  (make-instance 'funcl-function 
                 :domain (domain matrix)
                 :range (range matrix)
                 :lambda-function (lambda (arg) (transpose (evaluate matrix arg)))
                 :differentiator (lambda () (transpose (differentiate matrix)))))
(defmethod transpose ((matrix number)) matrix)
@export
(defgeneric complex-conjugate (arg))
(defmethod complex-conjugate ((arg number)) (conjugate arg))
(defmethod complex-conjugate ((arg simple-array)) (map 'vector 'conjugate arg))
(defmethod complex-conjugate ((arg magicl:matrix)) (magicl:conjugate-entrywise arg))
@export
(defgeneric dot-product (a b))

(defmethod dot-product ((a number) (b number)) (* a b))
(defmethod dot-product ((a simple-array) (b simple-array)) (reduce #'+ (map 'vector #'* a 
                                                                            (complex-conjugate b))))
(defmethod dot-product ((a magicl:matrix) (b simple-array)) (dot-product (magicl::matrix-data a) b))
(defmethod dot-product ((a simple-array) (b magicl:matrix)) (dot-product a (magicl::matrix-data b)))
(defmethod dot-product ((a magicl:matrix) (b magicl:matrix)) (dot-product (magicl::matrix-data a)
                                                                          (magicl::matrix-data b)))

(defgeneric real-part (arg))
(defmethod real-part ((arg number)) (realpart arg))
(defmethod real-part ((arg magicl:matrix))
  (* 1/2 (+ arg (magicl:conjugate-entrywise arg))))
(defmethod real-part ((arg funcl-function))
  (make-instance 'funcl-function
                 :differentiator (lambda () (real-part (differentiate arg)))
                 :lambda-function (lambda (time) (real-part (evaluate arg time)))
                 :range (range arg)
                 :domain (domain arg)))
