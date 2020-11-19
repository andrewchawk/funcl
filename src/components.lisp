(in-package :funcl)
(annot:enable-annot-syntax)

(defclass component-of (funcl-function)
  ((parent-function :accessor parent-function :initarg :parent-function)
   (place :accessor place :initarg :place)))

(defclass packed-vector (funcl-function)
  ((scalar-functions :accessor scalar-functions :initarg :scalar-functions)))

(defclass packed-matrix (funcl-function)
  ((scalar-functions :accessor scalar-functions :initarg :scalar-functions)
   (n :accessor n :initarg :n)
   (m :accessor m :initarg :m)))

(defmethod print-object ((packed-vector packed-vector) stream)
  (with-slots (scalar-functions) packed-vector
    (format stream "#<(~{~a~^, ~})>" (mapcar #'name scalar-functions))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmethod print-object ((packed-matrix packed-matrix) stream)
  (with-slots (scalar-functions n) packed-matrix
    (format stream "#<(~{(~{~a~^, ~})~^~%   ~})>" (group (mapcar #'name scalar-functions) n))))

@export
(defun vector-component (parent-function place)
  (if (typep parent-function 'packed-vector)
      (nth place (scalar-functions parent-function))
      (make-instance 'component-of
                     :parent-function parent-function
                     :place place
                     :domain 'scalar
                     :range (range parent-function)
                     :differentiator (lambda () (vector-component (differentiate parent-function) place))
                     :lambda-function (lambda (arg) (apply #'aref (evaluate parent-function arg) (list place))))))

@export
(defun pack-vector (&rest scalar-functions)
  (make-instance 'packed-vector
                 :scalar-functions scalar-functions
                 :domain (when scalar-functions (domain (first scalar-functions)))
                 :range  (append (list (length scalar-functions))
                                 (when scalar-functions
                                   (range (car scalar-functions))))
                 :differentiator (lambda () (apply #'pack-vector (mapcar #'differentiate scalar-functions))
                                   ;(apply #'differentiate (mapcar #'pack-vector scalar-functions))
                                   )
                 :lambda-function (lambda (arg) 
                                    (let ((unpermuted-result
                                            (aops:combine (map 'vector (point-evaluator arg)
                                                               scalar-functions))))
                                      #|(aops:permute (mapcar #'1- (perm:perm-to-list
                                                                  (perm:perm-transpose-indexes
                                                                   (perm:perm-identity (array-rank unpermuted-result))
                                                                   1 (array-rank unpermuted-result))))
                                                    unpermuted-result)|#
                                      (tensor-permute unpermuted-result
                                                      (perm-transpose (array-rank unpermuted-result)
                                                                      0 (1- (array-rank unpermuted-result))))))))

@export
(defun pack-matrix (n m &rest scalar-functions)
  (make-instance 'packed-matrix
                 :domain 'square-matrix
                 :range (when scalar-functions (range (first scalar-functions)))
                 :differentiator (lambda () (apply #'pack-matrix n m (mapcar #'differentiate scalar-functions)))
                 :scalar-functions scalar-functions
                 :n n :m m
                 :lambda-function (lambda (arg) (magicl:make-complex-matrix n m 
                                                                            (mapcar #'flatten-scalar
                                                                                    (mapcar
                                                                                     (point-evaluator arg)
                                                                                     scalar-functions))))))

@export
(defun matrix-component (parent-function place)
  (if (typep parent-function 'packed-matrix)
      (with-slots (n m scalar-functions) parent-function
        (nth (+ (* n (first place)) (second place)) scalar-functions))
      (make-instance 'component-of
                     :parent-function parent-function
                     :place place
                     :domain 'scalar
                     :range (range parent-function)
                     :differentiator (lambda () (component-of-matrix (differentiate parent-function) place))
                     :lambda-function (lambda (arg) 
                                        (realpart (apply #'magicl:ref (evaluate parent-function arg) place))))))

@export
(defun laplacian (function) (matrix-trace (differentiate (differentiate function))))

@export
(defun hessian (function)
  (simple-array->magicl-matrix (differentiate (differentiate function))))
