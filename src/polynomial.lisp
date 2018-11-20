(in-package :funcl)
(defun differentiate-polynomial-coefficients (coefficients)
  (if (< (length coefficients) 2)
      #(0)
      (map 'vector '* (alexandria:iota (1- (length coefficients)) :start 1)
           (make-array (1- (length coefficients)) :displaced-to coefficients :displaced-index-offset 1))))

(defun evaluate-polynomial (coefficients arg)
  (let ((n (1- (length coefficients))))
    (loop with b = (aref coefficients n)
       for i from (1- n) downto 0 
       do (setf b (+ (aref coefficients i) (* b arg)))
       finally (return b))))

(defclass polynomial-function (funcl-function)
  ((coefficients :accessor coefficients :initarg :coefficients)
   (domain :initform 'square-matrix)
   (range :initform 'square-matrix)))

(defun make-polynomial (coefficients)
  (make-instance 'polynomial-function
                 :coefficients coefficients
                 :domain 'square-matrix
                 :range 'square-matrix
                 :differentiator (lambda () (make-polynomial (differentiate-polynomial-coefficients coefficients)))
                 :lambda-function (lambda (arg) (evaluate-polynomial coefficients arg))))

(defun |#q-reader| (stream sub-char numarg) (make-polynomial (apply #'vector (read stream))))

(set-dispatch-macro-character #\# #\q #'|#q-reader|)

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

(defun nth-chebyshev-node (n) (aref *chebyshev-polynomials* n))
(defun chebyshev-series (vector) 
  (reduce #'+ (map 'vector #'* vector *chebyshev-polynomials*)))
