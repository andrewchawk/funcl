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

@export
(defgeneric solve-linear-system (a b))

(defmethod solve-linear-system ((a magicl:matrix) (b t))
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
   (reshaper :initarg :reshaper :initform (error "Need one of these"))
   (dispatcher :initarg :dispatcher :initform  (error "Need one of these"))
   (piece-function :initarg :piece-function :initform (error "Need one of these")))
  (:documentation "Interpolates a function over a grid."))

@export
(defun make-piecewise-function (dispatcher piece-function reshaper derivative-prefactor)
  (make-instance 'piecewise-function
                 :dispatcher dispatcher
                 :reshaper reshaper
                 :piece-function piece-function
                 :lambda-function (lambda (arg) (evaluate (funcall piece-function (funcall dispatcher arg)) (funcall reshaper arg)))
                 :differentiator (lambda () (make-piecewise-function
                                             dispatcher
                                             (lambda (arg) (* (aref derivative-prefactor 0)) (differentiate
                                                            (funcall piece-function arg)))
                                             reshaper derivative-prefactor))))

@export
(defun make-grid-interpolator (piece-function x-min dx x-length y-min dy y-length z-min dz z-length)
  (make-piecewise-function 
   (lambda (vector)
     (let ((x (aref vector 0)) ;; redesign this whole API! bug prone
           (y (aref vector 1))
           (z (aref vector 2)))
       (vector (floor (* dx (/ (- x x-min) x-length)))
               (floor (* dy (/ (- y y-min) y-length)))
               (floor (* dz (/ (- z z-min) z-length))))))
   piece-function
   (lambda (vector)
     (let ((x (aref vector 0))
           (y (aref vector 1))
           (z (aref vector 2)))
       (vector (* (/ dx x-length) (mod x (/ x-length dx)))
               (* (/ dy y-length) (mod y (/ y-length dy)))
               (* (/ dz z-length) (mod z (/ z-length dz))))))
   (vector (/ x-length dx) (/ dy y-length) (/ dz z-length))))

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

(defclass piecewise-function (funcl-function)
  ((domain :initform '(vector 3))
   (range :initform 'scalar)
   ;(reshaper :initarg :reshaper :initform (error "Need one of these"))
   ;'(dispatcher :initarg :dispatcher :initform  (error "Need one of these"))
   (piece-function :initarg :piece-function :initform (error "Need one of these")))
  (:documentation "Interpolates a function over a grid."))

@export
(defun make-piecewise-function (piece-function &key (domain '(3)) (range nil))
  (make-instance 'piecewise-function
                 :piece-function piece-function ;
                 :domain domain
                 :range range
                 :lambda-function (lambda (arg) (evaluate (funcall piece-function arg) arg))
                 :differentiator (lambda () (make-piecewise-function
                                             (lambda (arg) (differentiate (funcall piece-function arg))
                                               )
                                             :domain domain :range (append range domain)))))



@export
(defun make-grid-interpolator (grid-function x-min dx x-length y-min dy y-length z-min dz z-length)
  (let ((ijk (lambda (arg)
               (vector (floor (* dx (/ (- (aref arg 0) x-min) x-length)))
                       (floor (* dy (/ (- (aref arg 1) y-min) y-length)))
                       (floor (* dz (/ (- (aref arg 2) z-min) z-length)))))))
    (make-piecewise-function 
     (lambda (arg)
       (compose (funcall grid-function (funcall ijk arg))
                (pack-vector
                 (- #1=(multihomogeneous (/ dx x-length) 0 0)
                    (floor-funcl #1#))
                 (- #2=(multihomogeneous 0 (/ dy y-length) 0)
                    (floor-funcl #2#))
                 (- #3=(multihomogeneous 0 0 (/ dz z-length))
                    (floor-funcl #3#))))))))



(defun matrix-rank (matrix &key (epsilon 1e-6))
  (count 0 (cl-num-utils:diagonal-matrix-elements
            (lla:svd-d (lla:svd matrix)))
         :test (lambda (x y) (> (abs (- x y)) epsilon))))

(defun matrix-nullity (matrix &key (epsilon 1e-6)) ; rank-nullity theorem
  (- (cadr (array-dimensions matrix)) (matrix-rank matrix :epsilon epsilon)))

(defun flatten-coefficient-vector (coefficients)
  (aops:displace coefficients (reduce #'* (array-dimensions coefficients))))

(defun roll-coefficient-vector (coefficients &key (rank 3))
  (aops:displace coefficients (make-list rank :initial-element (round (expt (array-dimension coefficients 0) (/ rank))))))

(defun polynomial-basis-vectors (&key (m 4) (n 3))
  (map 'vector (lambda (i)
                 (make-multivariate-polynomial
                  (roll-coefficient-vector
                   #(0); (cl-irie::one-shot i :n (expt m n)) :rank n
                   )))
       (alexandria:iota (expt m n))))

(defun evaluator (point)
  (lambda (func)
    (evaluate func point)))

(defun polynomial-laplacian (polynomial)
  (reduce #'+ (loop for i from 0 below (array-rank (coefficients polynomial))
                    collecting (vector-component (differentiate (vector-component (differentiate polynomial) i)) i))))

(defun polynomial-laplacian-basis-matrix (&key (m 4) (n 3))
  (aops:combine
   (map 'vector (lambda (poly)
                  (flatten-coefficient-vector 
                   (coefficients
                    (polynomial-laplacian poly))))
        (polynomial-basis-vectors :m m :n n))))

(defun solve-underdetermined-system (a b)
  "Solves the system Ax=b such that |x| is minimised. Taken from Jim Lambers: Minimum Norm Solutions of Underdetermined Systems USM."
  (let* ((a-t (aops:permute '(1 0) a))
         (w (lla:solve (lla:mm a a-t) b)))
    (lla:mm a-t w)))
