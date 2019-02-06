(in-package :funcl)
(annot:enable-annot-syntax)


;; Tricubic interpolation routines

(defparameter *tricubic-interpolator-matrix*
  (magicl:inv (magicl:make-complex-matrix 
               64 64 
               (loop for i from 0 to 3 appending
                    (loop for j from 0 to 3 appending
                         (loop for k from 0 to 3 appending
                              (loop for x from -1 to 2 appending
                                   (loop for y from -1 to 2 appending
                                        (loop for z from -1 to 2 collecting
                                             (* (expt x i)
                                                (expt y j)
                                                (expt z k)))))))))))

(defun solve-upper-tridiagonal-system (r y)
  "Solves the tridiagonal system Rx=y by pivoting."
  (error "Not implemented yet"))

(defun solve-linear-system (a b)
  "Solves the linear system Ax=b via QR decomposition."
  (multiple-value-bind (q r)
      (magicl:qr a)
    (let ((y (magicl:multiply-complex-matrices (magicl:transpose q) b)))
      (solve-upper-tridiagonal-system r y))))

@export
(defun tricubic-interpolation (values)
  "Returns a tricubic interpolating function that is equal to `values' sampled on the 4x4x4 grid of points the innermost of which bound a cube of interest. values must be in the form of a 64x1 magicl matrix."
  (make-multivariate-polynomial 
   (make-array '(4 4 4)
               :displaced-to 
               (map 'vector #'realpart (magicl::matrix-data
                               (magicl:multiply-complex-matrices
                                *tricubic-interpolator-matrix*
                                values))))))

(defclass piecewise-function (funcl-function)
  ((domain :initform '(vector 3))
   (range :initform 'scalar)
   (dispatcher :initarg :dispatcher :initform  (error "Need one of these"))
   (piece-function :initarg :piece-function :initform (error "Need one of these")))
  (:documentation "Interpolates a function over a grid."))

@export
(defun make-piecewise-function (dispatcher piece-function)
  (make-instance 'piecewise-function
                 :dispatcher dispatcher
                 :piece-function piece-function
                 :lambda-function (lambda (arg) (evaluate (funcall piece-function (funcall dispatcher arg))
                                                          arg))
                 :differentiator (lambda () (make-piecewise-function
                                             dispatcher
                                             (lambda (arg) (differentiate
                                                            (funcall piece-function arg)))))))

@export
(defun make-grid-interpolator (piece-function x-min dx x-length y-min dy y-length z-min dz z-length)
  (make-piecewise-function 
   (lambda (vector)
     (let ((x (aref vector 0))
           (y (aref vector 1))
           (z (aref vector 2)))
       (vector (floor (* dx (/ (- x x-min) x-length)))
               (floor (* dy (/ (- y y-min) y-length)))
               (floor (* dz (/ (- z z-min) z-length))))))
   (lambda (arg)
     (funcall piece-function arg))))

;; Chebyshev interpolation routines

(defparameter *chebyshev-polynomials* 
  (coerce (loop for i from 0 to 100
             with p1 = (make-polynomial #(1))
             with p2 = (make-polynomial #(0 1))
             collecting p1
             do (psetf p1 p2
                       p2 (- (* p2 (make-polynomial #(0 2))) p1))) 'vector))

@export
(defun nth-chebyshev-node (n) (aref *chebyshev-polynomials* n))

@export
(defun chebyshev-series (vector) 
  "Returns a Chebyshev series $$\\Sigma^{N}_{i=1} a_i T_i(x) $$ where $$T_i(x)$$ are the Chebyshev polynomials of the first kind."
  (reduce #'+ (map 'vector #'* vector *chebyshev-polynomials*)))

