(in-package :funcl)
(annot:enable-annot-syntax)

@export
(defun differentiate-polynomial-coefficients (coefficients)
  "Implements polynomial differentation. coefficients must be of type vector arranged in order of increasing degree e.g. $$2x^2 + 4x + 1$$ is represented by #(1 4 2)."
  (if (< (length coefficients) 2)
      #(0)
      (map 'vector '* (alexandria:iota (1- (length coefficients)) :start 1)
           (make-array (1- (length coefficients)) :displaced-to coefficients :displaced-index-offset 1))))

@export
(defun evaluate-polynomial (coefficients arg)
  "Evaluates a polynomial at arg using Horner's method. coefficients must be of type vector arranged in order of increasing degree e.g. $$2x^2 + 4x + 1$$ is represented by #(1 4 2)."
  (let ((n (1- (length coefficients))))
    (loop with b = (aref coefficients n)
       for i from (1- n) downto 0 
       do (setf b (+ (aref coefficients i) (* b arg)))
       finally (return b))))

@export-class
(defclass polynomial-function (funcl-function)
  ((coefficients :accessor coefficients :initarg :coefficients)
   (domain :initform 'square-matrix)
   (range :initform 'square-matrix))
  (:documentation "Represents a polynomial. coefficients must be of type vector arranged in order of increasing degree e.g. $$2x^2 + 4x + 1$$ is represented by #(1 4 2)."))

@export
(defun make-polynomial (coefficients)
  (make-instance 'polynomial-function
                 :coefficients coefficients
                 :domain 'square-matrix
                 :range 'square-matrix
                 :differentiator (lambda () (make-polynomial (differentiate-polynomial-coefficients coefficients)))
                 :lambda-function (lambda (arg) (evaluate-polynomial coefficients arg))))

(defun |#q-reader| (stream &optional sub-char numarg)
  @ignore sub-char numarg
  (make-polynomial (apply #'vector (read stream))))

@export
(defun monomial (n) (make-polynomial (coerce (append (make-list n :initial-element 0) '(1)) 'vector)))

(defmethod print-object ((object polynomial-function) stream)
  (format stream "#q(~{~s~^ ~})" (coerce (coefficients object) 'list)))

(defun ensure-length (array n &key (initial-element 0))
  (if (< (length array) n)
      (let ((result (make-array (list n) :initial-element initial-element)))
        (loop for i from 0 below (length array) do (setf (aref result i) (aref array i)) finally (return result)))
      array))

(defun ensure-length-of (array target &key (initial-element 0)) (ensure-length array (length target) :initial-element initial-element))

(defun multiply-polynomials (u v)
  (let* ((degree-a (1- (length u)))
         (degree-b (1- (length v)))
         (degree-product (+ degree-a degree-b))
         (a (ensure-length u (1+ degree-product)))
         (b (ensure-length v (1+ degree-product)))
         (result (make-array (list (1+ degree-product)) :initial-element 0)))
    (loop for i from 0 to degree-product 
       do (setf (aref result i) 
                (loop for j from 0 to i summing 
                     (* (aref a j)
                        (aref b (- i j)))))
       finally (return result))))

(defun add-polynomials (a b) (map 'vector #'+ (ensure-length-of a b) (ensure-length-of b a)))
(defun scale-polynomial (a scale) (map 'vector (alexandria:curry #'* scale) a))

@export
(defun constant (a) (make-polynomial (vector a)))

(bld-gen:defmeth2 + ((a polynomial-function) (b polynomial-function))
  (make-polynomial (add-polynomials (coefficients a) (coefficients b))))

(bld-gen:defmeth2 * ((a polynomial-function) (b polynomial-function))
  (make-polynomial (multiply-polynomials (coefficients a) (coefficients b))))

(bld-gen:defmeth2 * ((a polynomial-function) (b number)) 
  (make-polynomial (scale-polynomial (coefficients a) b)))

(bld-gen:defmeth1 - ((a polynomial-function)) 
  (make-polynomial (scale-polynomial (coefficients a) -1)))

(bld-gen:defmeth2 - ((a polynomial-function) (b polynomial-function)) 
  (+ a (- b)))

(defparameter *chebyshev-polynomials* (coerce (loop for i from 0 to 100
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