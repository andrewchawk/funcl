(in-package :funcl)
(annot:enable-annot-syntax)

(defclass component-of (funcl-function)
  ((parent-function :accessor parent-function :initarg :parent-function)
   (place :accessor place :initarg :place)))

@export
(defun vector-component (parent-function place)
  (make-instance 'component-of
                 :parent-function parent-function
                 :place place
                 :domain 'scalar
                 :range (range parent-function)
                 :differentiator (lambda () (component-of (differentiate parent-function) place))
                 :lambda-function (lambda (arg) (apply #'aref (evaluate parent-function arg) (list place)))))

@export
(defun pack-vector (&rest scalar-functions)
  (make-instance 'funcl-function
                 :domain 'vector
                 :range (when scalar-functions (range (first scalar-functions)))
                 :differentiator (lambda () (apply #'pack-scalars (mapcar #'differentiate scalar-functions)))
                 :lambda-function (lambda (arg) (map 'vector (point-evaluator arg)
                                                     scalar-functions))))

@export
(defun pack-matrix (n m &rest scalar-functions)
  (make-instance 'funcl-function
                 :domain 'square-matrix
                 :range (when scalar-functions (range (first scalar-functions)))
                 :differentiator (lambda () (apply #'pack-matrix n m scalar-functions))
                 :lambda-function (lambda (arg) (magicl:make-complex-matrix n m 
                                                                            (mapcar
                                                                             (point-evaluator arg)
                                                                             scalar-functions)))))

@export
(defun matrix-component (parent-function place)
  (make-instance 'component-of
                 :parent-function parent-function
                 :place place
                 :domain 'scalar
                 :range (range parent-function)
                 :differentiator (lambda () (component-of-matrix (differentiate parent-function) place))
                 :lambda-function (lambda (arg) 
                                    (realpart (apply #'magicl:ref (evaluate parent-function arg) place)))))
