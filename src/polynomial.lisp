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
 ; (break)
  (let ((n (1- (length coefficients))))
    (loop with b = (aref coefficients n)
       for i from (1- n) downto 0 
       do (setf b (+ (aref coefficients i) (* b arg)))
       finally (return b))))

@export-class
(defclass multivariate-polynomial (funcl-function)
  ((coefficients :accessor coefficients :initarg :coefficients)
   (domain :initform 'square-matrix)
   (range :initform 'square-matrix))
  (:documentation "Represents a multivariate polynomial."))

@export-class
(defclass polynomial-function (multivariate-polynomial)
  ()
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
 ; @ignore sub-char
  (if numarg
      (let* ((multidimensional-list (read stream))
             (dimensions (loop with list = multidimensional-list
                            until (atom list)
                            collecting (length list)
                            do (setf list (car list)))))
        (assert (eq numarg (length dimensions)))
        (make-multivariate-polynomial
         (make-array dimensions :initial-contents multidimensional-list)))
      (make-polynomial (apply #'vector (read stream)))))



@export
(defun monomial (n) (make-polynomial (coerce (append (make-list n :initial-element 0) '(1)) 'vector)))

(defmethod print-object ((object polynomial-function) stream)
  (format stream "#Q(~{~s~^ ~})" (coerce (coefficients object) 'list)))


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
(bld-gen:defmeth2 + ((a multivariate-polynomial) (b multivariate-polynomial))
  (make-multivariate-polynomial (add-multivariate-polynomial-coefficients (coefficients a) (coefficients b))))

(bld-gen:defmeth2 * ((a multivariate-polynomial) (b multivariate-polynomial))
  (make-multivariate-polynomial (multiply-multivariate-polynomial-coefficients (coefficients a) (coefficients b))))

(bld-gen:defmeth2 * ((a multivariate-polynomial) (b number)) 
  (make-multivariate-polynomial (scalar-multiply-multivariate-polynomial-coefficients (coefficients a) b)))

(bld-gen:defmeth2 * ((a number) (b multivariate-polynomial)) (* b a))

(bld-gen:defmeth1 - ((a polynomial-function)) 
  (make-multivariate-polynomial (scalar-multiply-multivariate-polynomial-coefficients (coefficients a) -1)))

(bld-gen:defmeth2 - ((a multivariate-polynomial) (b multivariate-polynomial)) 
  (+ a (- b)))

@export
(defun nth-chebyshev-node (n) (aref *chebyshev-polynomials* n))

@export
(defun chebyshev-series (vector) 
  "Returns a Chebyshev series $$\\Sigma^{N}_{i=1} a_i T_i(x) $$ where $$T_i(x)$$ are the Chebyshev polynomials of the first kind."
  (reduce #'+ (map 'vector #'* vector *chebyshev-polynomials*)))

@export
(defun evaluate-multivariate-polynomial (coefficients arg)
  (let* ((number-of-variables (length arg))
         (lowest-degree (array-dimension coefficients 0))
         (degrees (array-dimensions coefficients))
         (new-degrees (cdr degrees))
         (lowest-arg (aref arg 0)))
    (if (= 1 number-of-variables)
        (evaluate-polynomial coefficients lowest-arg)
        (let ((new-coefficients 
               (make-array (list lowest-degree) :initial-element 0)))
          (loop for i from 0 below lowest-degree
               do (setf (aref new-coefficients i)
                        (evaluate-multivariate-polynomial 
                         (make-array new-degrees
                                     :displaced-to coefficients
                                     :displaced-index-offset 
                                     (* i (reduce '* new-degrees)))
                         (make-array (1- number-of-variables)
                                     :displaced-to arg
                                     :displaced-index-offset 1))))
          (evaluate-polynomial new-coefficients lowest-arg)))))

(defmethod print-object ((object multivariate-polynomial) stream)
  (if (typep (coefficients object) 'sequence)
      (format stream "#Q(~{~s~^ ~})" (coerce (coefficients object) 'list))
      (format stream "~A" (substitute #\Q #\A (format nil "~s" (coefficients object))))))

@export
(defun differentiate-multivariate-polynomial (coefficients index)
  (let* ((coefficients-dimensions (array-dimensions coefficients))
         (number-of-variables (array-rank coefficients))
         (final-degree-of-index (1- (nth index coefficients-dimensions)))
         (new-coefficients-dimensions (loop for i from 0 below number-of-variables
                                         collecting (if (eq i index)
                                                        final-degree-of-index
                                                        (nth i coefficients-dimensions))))
         (new-coefficients-rank (reduce #'* new-coefficients-dimensions))
         (new-coefficients (make-array new-coefficients-dimensions)))
    (loop for i from 0 below new-coefficients-rank
       with current-ref = (make-list number-of-variables :initial-element 0)
       do (progn
            (setf (apply #'aref new-coefficients current-ref)
                  (* (1+ (nth index current-ref))
                     (apply #'aref coefficients (jump-ref current-ref index))))
            (if (eq i (1- new-coefficients-rank))
                (return new-coefficients)
                (setf current-ref (increment current-ref new-coefficients-dimensions)))))))


@export 
(defun gradient-coefficients (coefficients)
  (coerce (loop for i from 0 below (array-rank coefficients)
                     collecting (differentiate-multivariate-polynomial coefficients i)) 'vector))

@export
(defun gradient (multivariate-polynomial)
  (map 'vector #'make-multivariate-polynomial (gradient-coefficients (coefficients multivariate-polynomial))))

@export
(defun gradient-squared (multivariate-polynomial)
  (let ((gradient (gradient multivariate-polynomial)))
    (reduce #'+ (loop for i from 0 below (length gradient)
                   collecting (* (aref gradient i) (aref gradient i))))))

@export
(defun laplacian (multivariate-polynomial)
  (with-slots (coefficients) multivariate-polynomial
    (loop for i from 0 below (array-rank coefficients)
       collecting (differentiate-multivariate-polynomial
                   (differentiate-multivariate-polynomial
                    coefficients i) i))))

@export
(defun make-multivariate-polynomial (coefficients)
  "Makes a multivariate polynomial with the coefficients."
  (if (typep coefficients 'sequence)
      (make-polynomial coefficients)
      (make-instance 'multivariate-polynomial 
                     :coefficients coefficients
                     :lambda-function (lambda (arg) (evaluate-multivariate-polynomial coefficients arg))
                     :differentiator (lambda () (apply #'values 
                                                       (loop for i from 0 below (array-rank coefficients)
                                                          collecting
                                                            (make-multivariate-polynomial
                                                             (differentiate-multivariate-polynomial
                                                              coefficients i))))))))

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
  "Returns a tricubic interpolating function that is equal to `values' sampled on the 4x4x4 grid of points the innermost of which bound a cube of interest."
  (make-multivariate-polynomial 
   (make-array '(4 4 4)
             ;  :element-type 'double-float
               :displaced-to 
               (map 'vector #'realpart (magicl::matrix-data
                               (magicl:multiply-complex-matrices
                                *tricubic-interpolator-matrix*
                                (magicl:make-complex-matrix 64 1 values)))))))



(defun ensure-multivariate-polynomial-coefficients-length-of (source target)
  "Returns a new array of coefficients that are that of source, padded with zeroes to be at least the size of target."
  (let* ((result-dimensions (mapcar #'max 
                                    (ensure-list-length (array-dimensions target) (array-rank source))
                                    (ensure-list-length (array-dimensions source) (array-rank target))))
         (result (make-array result-dimensions :initial-element 0)))
    (multidimensional-loop
     (result place)
     when (place-within-array-p place source)
     do (setf (apply #'aref result place)
              (apply #'aref source (loop for i from 0 below (array-rank source)
                                      for j in place collecting j)))
     finally (return result))))


(defun multiply-multivariate-polynomial-coefficients (c1 c2)
  (let* ((c1-dimensions (array-dimensions c1))
         (c2-dimensions (array-dimensions c2))
         (result (make-array 
                  (mapcar #'1- (mapcar #'+ 
                                       (ensure-list-length-of c1-dimensions c2-dimensions :initial-element 1)
                                       (ensure-list-length-of c2-dimensions c1-dimensions :initial-element 1))) 
                             :initial-element 0)))
    ;(format t "Result dimension: ~s~%" (array-dimensions result))
    (multidimensional-loop 
     (result result-place)
     do (setf (apply #'aref result result-place)
              (multidimensional-loop
               (c1 place1)
               when (place-in-array-p (mapcar #'- result-place (ensure-list-length-of place1 result-place)) c2)
               sum (* (apply #'aref c1 place1)
                      (apply #'aref c2 (mapcar #'- result-place (ensure-list-length-of place1 result-place))))))
     finally (return result))))

(defun add-multivariate-polynomial-coefficients (c1 c2)
  (array-map #'+
             (ensure-multivariate-polynomial-coefficients-length-of c1 c2)
             (ensure-multivariate-polynomial-coefficients-length-of c2 c1)))

(defun scalar-multiply-multivariate-polynomial-coefficients (scalar c)
  (array-map (lambda (coefficient) (* scalar coefficient)) c))

(defun differentiate-multivariate-polynomial (coefficients index)
  (let ((result (make-array (jump-place (array-dimensions coefficients) index -1)
                            :initial-element 0)))
    (multidimensional-loop 
     (result place)
     when (place-within-array-p (jump-place place index) coefficients)
     do (setf (apply #'aref result place)
              (* (1+ (nth index place))
                 (apply #'aref coefficients (jump-place place index))))
     
     finally (return result))))
#|
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

|#

#||

(defparameter *chebyshev-polynomials* (coerce (loop for i from 0 to 100
                                                 with p1 = (make-polynomial #(1))
                                                 with p2 = (make-polynomial #(0 1))
                                                 collecting p1
                                                 do (psetf p1 p2
                                                           p2 (- (* p2 (make-polynomial #(0 2))) p1))) 'vector))

|#
