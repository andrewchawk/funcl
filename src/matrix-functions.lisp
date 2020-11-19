(in-package :funcl)
(annot:enable-annot-syntax)

@export
(defun random-scalar (&key (n 3) (range 3) (duration 1.0d0))
  (compose (chebyshev-series (- range (* 2 range (aops:rand (list n)))))
           (make-polynomial `#(0 ,(/ duration)))))

@export
(defun random-matrix (m n)
  (apply #'pack-matrix m n (loop repeat (* n m) collect (random-scalar))))

@export
(defun random-lower-triangular-matrix-function (n)
  (apply #'pack-matrix n n
         (loop for i below n appending
               (loop for j below n collecting
                     (if (> i j)
                         (constant 0)
                         (random-scalar))))))

@export
(defun random-upper-triangular-matrix-function (n)
  (transpose (random-lower-triangular-matrix-function n)))

@export
(defun random-positive-matrix-function (n)
  (let ((cholesky-factor (random-lower-triangular-matrix-function n)))
    (values (* cholesky-factor (transpose cholesky-factor))
            cholesky-factor)))

@export
(defun random-symmetric-matrix-function (n)
  (let ((lower-triangular-matrix (random-lower-triangular-matrix-function n)))
    (+ lower-triangular-matrix (transpose lower-triangular-matrix))))

@export
(defun identity-matrix (n) (aops:generate (lambda (subscripts)
                                            (if (= (car subscripts) (cadr subscripts))
                                                1 0)) (list n n) :subscripts))

@export
(defun random-positive-matrix-function-from-identity (n &key (t-f 9) (scale 10))
  "Returns a random positive matrix M(t) where M(0)=I,M'(0)=0."
  (+ (simple-array->magicl-matrix (identity-matrix n))
     (* (make-polynomial #(0 0 0 1))
        scale
        (make-polynomial `#(,(- (expt t-f 3))
                            ,(* 3 t-f t-f)
                            ,(* -3 t-f)
                            1))
        (random-positive-matrix-function n))))

(defun to-diagonal (diagonal)
  (aops:generate (lambda (index)
                   (if (= (car index) (cadr index))
                       (aref diagonal (car index))
                       0))
                 (list (array-dimension diagonal 0)
                       (array-dimension diagonal 0))
                 :subscripts))

(defgeneric conjugate-transpose (array))

(defmethod conjugate-transpose ((array array))
  (aops:each #'conjugate (aops:permute '(1 0) array)))

(defmethod conjugate-transpose ((array magicl:matrix))
  (conjugate-transpose (magicl-matrix->multidimensional-array array)))

(defmethod conjugate-transpose ((function funcl-function))
  (make-instance 'combination-function
                 :function-1 function
                 :range (range function)
                 :domain (domain function)
                 :function-2 nil
                 :differentiator (lambda () (conjugate-transpose (differentiate function)))
                 :lambda-function (lambda (arg)
                              (conjugate-transpose (evaluate function arg)))))

@export
(defun eigendecomposition (m)
  ;; (let* ((svd (lla:svd m))
  ;;        (o (lla::svd-u svd))
  ;;        (diagonal (* (transpose o) m o)))
  ;;   (values o diagonal (* o diagonal (conjugate-transpose o))))
  (multiple-value-bind (diagonal u)
      (magicl:eig (simple-array->magicl-matrix m))
    (values (conjugate-transpose (magicl-matrix->multidimensional-array u))
            (to-diagonal diagonal)
            (lla:mmm (conjugate-transpose (magicl-matrix->multidimensional-array u))
                     (to-diagonal diagonal)
                     (magicl-matrix->multidimensional-array u)))))

@export
(defun test-matrix-function (function &key (t-f 1))
  (funcall #'format t "~a ~%" (lparallel:pmapcar (lambda (time)
                                              (list time (evaluate function time)))
                                            (loop for time from 0 to t-f by (/ t-f 5) collecting time))))

@export
(defmethod solve-anticommutator ((a array) (b array))
  "Solves the matrix anticommutator equation AX + XA =B for X given A and B (c.f. Robert Lewis stack exchange)"
  (multiple-value-bind (u diagonal)
      (eigendecomposition a)
    (let* ((c (lla:mmm (conjugate-transpose u) b u))
           (y (aops:generate
               (lambda (index)
                 (let ((i (car index))
                       (j (cadr index)))
                   (/ (aref c i j) (+ (aref diagonal i i)
                                      (aref diagonal j j)))))
               (aops:dims a) :subscripts))
           (x (lla:mmm u y (funcl::conjugate-transpose u))))
      (values x ;(+ (lla:mmm a x) (lla:mmm x a))
              ))))

(defmethod solve-anticommutator ((a magicl:matrix) (b magicl:matrix))
  (solve-anticommutator (magicl-matrix->multidimensional-array a)
                        (magicl-matrix->multidimensional-array b)))

(defclass anticommutator-solution (funcl-function)
  ((a :accessor a :initarg :a)
   (b :accessor b :initarg :b)))

(defmethod solve-anticommutator ((a funcl-function) (b funcl-function))
  (make-instance 'anticommutator-solution
                 :a a
                 :b b
                 :lambda-function (lambda (arg)
                                    (solve-anticommutator
                                     (evaluate a arg) (evaluate b arg)))
                 :domain 'square-matrix
                 :range 'square-matrix
                 :differentiator
                 (lambda () (solve-anticommutator a
                                                  (- b
                                                     (* (differentiate a) (solve-anticommutator a b))
                                                     (* (solve-anticommutator a b) (differentiate a)))))))

(defun symmetrise (matrix)
  (* 1/2 (+ matrix (conjugate-transpose matrix))))

(defun antisymmetrise (matrix)
  (* 1/2 (- matrix (conjugate-transpose matrix))))
