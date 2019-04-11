(in-package :funcl)
(annot:enable-annot-syntax)

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
                 :domain nil
                 :range nil
                 :differentiator (lambda ()
                                   (make-polynomial
                                    (differentiate-multivariate-polynomial coefficients 0)))
                 :lambda-function (lambda (arg) (evaluate-polynomial coefficients arg))))

@export
(defun monomial (n)
  (make-polynomial (coerce (append (make-list n :initial-element 0) '(1)) 'vector)))

@export
(defun constant (a) (make-polynomial (vector a)))

@export
(defun evaluate-polynomial (coefficients arg)
  "Evaluates a polynomial at arg using Horner's method. coefficients must be of type vector arranged in order of increasing degree e.g. $$2x^2 + 4x + 1$$ is represented by #(1 4 2)."
  (if (zerop (length coefficients))
      0
      (let ((n (1- (length coefficients))))
        (loop with b = (aref coefficients n)
              for i from (1- n) downto 0 
              do (setf b (+ (aref coefficients i) (* b arg)))
              finally (return b)))))

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
                                     :element-type (array-element-type coefficients)
                                     :displaced-index-offset 
                                     (* i (reduce '* new-degrees)))
                         (make-array (1- number-of-variables)
                                     :displaced-to arg
                                     :element-type (array-element-type arg)
                                     :displaced-index-offset 1))))
          (evaluate-polynomial new-coefficients lowest-arg)))))


@export
(defun make-multivariate-polynomial (coefficients)
  "Makes a multivariate polynomial with the coefficients."
  (if (typep coefficients 'sequence)
      (make-polynomial coefficients)
      (make-instance 'multivariate-polynomial 
                     :coefficients coefficients
                      :lambda-function (lambda (arg)
                                         (evaluate-multivariate-polynomial coefficients arg))
                     :differentiator (lambda () (apply #'pack-vector
                                                       (loop for i from 0 below
                                                            (array-rank coefficients)
                                                          collecting
                                                            (make-multivariate-polynomial
                                                             (differentiate-multivariate-polynomial
                                                              coefficients i)))))
                     :range nil
                     :domain (list (array-rank coefficients)))))

(defun ensure-multivariate-polynomial-coefficients-length-of (source target)
  "Returns a new array of coefficients that are that of source, padded with zeroes to be at least the size of target."
  (let* ((result-dimensions (mapcar #'max 
                                    (ensure-list-length (array-dimensions target) 
                                                        (array-rank source))
                                    (ensure-list-length (array-dimensions source) 
                                                        (array-rank target))))
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
                  (mapcar #'1- 
                          (mapcar #'+ 
                                  (ensure-list-length-of c1-dimensions c2-dimensions
                                                         :initial-element 1)
                                  (ensure-list-length-of c2-dimensions c1-dimensions 
                                                         :initial-element 1))) 
                             :initial-element 0)))
    ;(format t "Result dimension: ~s~%" (array-dimensions result))
    (multidimensional-loop 
     (result result-place)
     do (setf (apply #'aref result result-place)
              (multidimensional-loop
               (c1 place1)
               when (place-in-array-p (mapcar #'- result-place 
                                              (ensure-list-length-of place1 result-place)) c2)
               sum (* (apply #'aref c1 place1)
                      (apply #'aref c2 (mapcar #'- result-place 
                                               (ensure-list-length-of place1 result-place))))))
     finally (return result))))

(defun add-multivariate-polynomial-coefficients (c1 c2)
  (array-map #'+
             (ensure-multivariate-polynomial-coefficients-length-of c1 c2)
             (ensure-multivariate-polynomial-coefficients-length-of c2 c1)))

(defun scalar-multiply-multivariate-polynomial-coefficients (scalar c)
  (array-map (lambda (coefficient) (* scalar coefficient)) c))

(defun differentiate-multivariate-polynomial (coefficients index)
 ; (format t "coefficients:~a, array-dimensions: ~a~%~%" coefficients (array-dimensions coefficients))
  (if (or (equal (array-dimensions coefficients) '(1))
          (zerop (reduce #'* (array-dimensions coefficients))))
      (make-array (make-list (array-rank coefficients) :initial-element 0))
      (let ((result (make-array (jump-place (array-dimensions coefficients) index -1)
                                :initial-element 0)))
        (multidimensional-loop 
         (result place)
         when (place-within-array-p (jump-place place index) coefficients)
         do (setf (apply #'aref result place)
                  (* (1+ (nth index place))
                     (apply #'aref coefficients (jump-place place index))))
         finally (return result)))))

@export 
(defun gradient-coefficients (coefficients)
  (coerce (loop for i from 0 below (array-rank coefficients)
                     collecting (differentiate-multivariate-polynomial coefficients i)) 'vector))

@export
(defun partial-derivative (multivariate-polynomial index)
  (make-multivariate-polynomial (differentiate-multivariate-polynomial (coefficients multivariate-polynomial) index)))

@export
(defun gradient (multivariate-polynomial)
  (map 'vector #'make-multivariate-polynomial 
       (gradient-coefficients (coefficients multivariate-polynomial))))

@export
(defun gradient-squared (multivariate-polynomial)
  (let ((gradient (gradient multivariate-polynomial)))
    (reduce #'+ (loop for i from 0 below (length gradient)
                   collecting (* (aref gradient i) (aref gradient i))))))

@export
(defun laplacian (multivariate-polynomial)
  (with-slots (coefficients) multivariate-polynomial
    (make-multivariate-polynomial 
     (reduce #'add-multivariate-polynomial-coefficients 
             (loop for i from 0 below (array-rank coefficients)
                collecting (differentiate-multivariate-polynomial
                            (differentiate-multivariate-polynomial
                             coefficients i) i))))))

@export
(defun polynomial= (function-1 function-2)
  (apply #'all 
         (coerce (flatten-array (array-map #'= (coefficients function-1) (coefficients function-2))) 'list)))
@export
(defun multihomogeneous (&rest scales)
  "Returns a multivariate polynomial ax + by + ... + cz where the coefficients a,b,...,c are taken from scales."
  (let ((coefficients (make-array (make-list (length scales) :initial-element 2))))
    (multidimensional-loop (coefficients place)
                           with variable = (1- (length scales))
                           do (when (= 1 (reduce #'+ place))
                                (setf (apply #'aref coefficients place) (nth variable scales))
                                (decf variable))
                           finally (return (make-multivariate-polynomial coefficients)))))

(defclass constant-function (funcl-function)
  ())

(defun constant (a &key (domain nil))
  (make-instance 'constant-function
                 :lambda-function (lambda (arg) (declare (ignore arg)) a)
                 :differentiator (lambda ()
                                   (constant (aops:zeros (append domain (unless (numberp a) (array-dimensions a)) ) ) :domain domain))
                 :range (unless (numberp a) (array-dimensions a)); (append domain (unless (numberp a) (array-dimensions a)) )
                 :domain domain)) 
