(in-package :funcl)
(cl-annot:enable-annot-syntax)

(bld-gen:defmeth2 + ((a magicl:matrix) (b magicl:matrix))
  (let ((n (magicl:matrix-rows a))
        (m (magicl:matrix-cols b))
        (unrolled-result (map 'vector '+ (magicl::matrix-data a) (magicl::matrix-data b))))
    (magicl::make-matrix :rows n :cols m :data (coerce unrolled-result `(simple-array ,(array-element-type (magicl::matrix-data a))
                                                                                      (,(* n m)))))))
@export
(defgeneric simple-array->magicl-matrix (b))

(defmethod simple-array->magicl-matrix ((b array))
  (if (= 2 (array-rank b))
      (magicl:make-complex-matrix
       #1=(car (array-dimensions b)) #2=(cadr (array-dimensions b))
       (loop for i from 0 below #1#
             appending (loop for j from 0 below #2#
                             collecting (aref b i j))))
      (when (< 0 (length b))
        (if (typep (aref b 0) 'simple-array)
            (magicl:make-complex-matrix (length b) (length (aref b 0))
                                        (loop for i from 0 below (length b)
                                              appending (loop for j from 0 below (length (aref b 0))
                                                              collecting (aref (aref b i) j))))
            (magicl:make-complex-matrix (length b) 1 (coerce b 'list))))))

(defmethod simple-array->magicl-matrix ((b funcl-function))
  (let ((lambda-function (slot-value b 'lambda-function)))
    (setf (slot-value b 'lambda-function) (lambda (arg) (simple-array->magicl-matrix
                                                         (funcall lambda-function arg)))))
  b)

(bld-gen:defmeth2 * ((a magicl:matrix) (b magicl:matrix)) (magicl:multiply-complex-matrices a b))
(bld-gen:defmeth2 * ((a magicl:matrix) (b number)) (magicl:scale b a))
(bld-gen:defmeth2 * ((a number) (b magicl:matrix)) (* b a))
(bld-gen:defmeth2 * ((a magicl:matrix) (b array))
  (* a (simple-array->magicl-matrix b)))

(bld-gen:defmeth2 * ((a array) (b magicl:matrix))
  (* (simple-array->magicl-matrix a) b))

(bld-gen:defmeth2 * ((a funcl-function) (b magicl:matrix)) (* a (constant b)))
(bld-gen:defmeth2 * ((a magicl:matrix) (b funcl-function)) (* (constant a) b))
(bld-gen:defmeth2 * ((a funcl-function) (b simple-array)) (* a (constant b)))
(bld-gen:defmeth2 * ((a simple-array) (b funcl-function)) (* (constant (simple-array->magicl-matrix a)) b))


(bld-gen:defmeth2 + ((a funcl-function) (b simple-array)) (+ a (constant b)))
(bld-gen:defmeth2 + ((a simple-array) (b funcl-function)) (+ b (constant a)))
;; (bld-gen:defmeth2 * ((a simple-array) (b simple-array))
;;   (if (array-dimensions a)
;;       (magicl:make-complex-matrix (array-0 a) (length b)
;;                                   (loop for i from 0 below (length a)
;;                                         appending
;;                                         (loop for j from 0 below (length b)
;;                                               collecting (* (aref a i) (aref b j)))))
;;       (* (aref a) b)))

(bld-gen:defmeth2 * ((a simple-array) (b simple-array))
  (if (array-dimensions a)
      (if (array-dimensions b)
          (lla:mm a b)
          (* a (aref b)))
      (* (aref a) b)))

(bld-gen:defmeth2 * ((a array) (b number)) (* b a))
(bld-gen:defmeth2 * ((a number) (b array))
  (if (array-dimensions b)
      (aops:each (lambda (arg) (* a arg)) b)
      (* a (aref b))))

(bld-gen:defmeth2 - ((a array) (b magicl:matrix))
  (- (simple-array->magicl-matrix a) b))

(bld-gen:defmeth2 - ((a magicl:matrix) (b array))
  (- a (simple-array->magicl-matrix b)))

(bld-gen:defmeth2 + ((a array) (b magicl:matrix))
  (+ (simple-array->magicl-matrix a) b))

(bld-gen:defmeth2 + ((a magicl:matrix) (b array))
  (+ b a))


(bld-gen:defmeth1 - ((a magicl:matrix)) (* -1 a))
(bld-gen:defmeth1 - ((a array)) (aops:each #'- a))
(bld-gen:defmeth2 - ((a magicl:matrix) (b magicl:matrix)) (+ a (- b)))
(bld-gen:defmeth1 / ((a magicl:matrix)) (magicl:inv a))
(bld-gen:defmeth1 / ((a array)) (lla:invert a))
(bld-gen:defmeth2 / ((a simple-array) (b number)) (* a (/ b)))
(bld-gen:defmeth2 / ((a number) (b simple-array)) (* a (/ b)))
(bld-gen:defmeth2 / ((a magicl:matrix) (b number)) (* a (/ b)))

(defmethod bld-ode:norminfx ((a magicl:matrix)) (bld-ode:norminfx (magicl::matrix-data a)))
(defmethod bld-ode:norminfx ((a array)) (bld-ode:norminfx (aops:flatten a)))

(defmethod expt ((a magicl:matrix) (b fixnum)) (magicl:exptm a b))

(defclass combination-function (funcl-function)
  ((combination-operation :accessor combination-operation :initarg :combination-operation)
   (function-1 :accessor function-1 :initarg :function-1)
   (function-2 :accessor function-2 :initarg :function-2)))

(defmethod name ((function combination-function))
  (let ((name-1 (when (slot-boundp function 'function-1) (name (function-1 function))))
        (name-2 (when (slot-boundp function 'function-2) (name (function-2 function)))))
    (alexandria:switch ((combination-operation function) :test #'eq)
      ('+ (format nil "~d + ~d" name-1 name-2))
      ('* (format nil "~d * ~d" name-1 name-2))
      ('- (format nil "~d - ~d" name-1 name-2))
      ('/ (format nil "~d / ~d" name-1 name-2))
      (#'dot-product (format nil "~d . ~d" name-1 name-2))
      ('compose (format nil "~d.~d" name-1 name-2)))))

(defmethod name ((function number)) (format nil "~,3f" function))
(defmethod name ((function magicl:matrix)) (format nil "~a" function))

(defclass named-function (funcl-function)
  ((name :accessor name :initarg :name)))

@export
(defun rename (function name)
  (change-class function 'named-function :name name))

(bld-gen:defmeth2 + ((a funcl-function) (b funcl-function))
  (make-instance 'combination-function 
                 :combination-operation '+ 
                 :function-1 a
                 :range (range a)
                 :domain (domain a)
                 :function-2 b
                 :differentiator (lambda () (+ (differentiate a) (differentiate b)))
                 :lambda-function (lambda (arg) (+ (evaluate a arg) (evaluate b arg)))))

(bld-gen:defmeth2 - ((a funcl-function) (b funcl-function))
  (make-instance 'combination-function 
                 :combination-operation '- 
                 :function-1 a 
                 :function-2 b
                 :range (range a)
                 :domain (domain a)
                 :differentiator (lambda () (- (differentiate a) (differentiate b)))
                 :lambda-function (lambda (arg) (- (evaluate a arg) (evaluate b arg)))))

(bld-gen:defmeth1 - ((a funcl-function))
  (make-instance 'funcl-function
                 :range (range a)
                 :domain (domain a)
                 :differentiator (lambda () (- (differentiate a)))
                 :lambda-function (lambda (arg) (- (evaluate a arg)))))

(bld-gen:defmeth2 * ((a funcl-function) (b funcl-function))
  (make-instance 'combination-function 
                 :combination-operation '*
                 :function-1 a
                 :function-2 b
                 :range (range a)
                 :domain (domain a)
                 :differentiator (lambda () (+ (* (differentiate a) b) (* a (differentiate b))))
                 :lambda-function (lambda (arg) (* (evaluate a arg) (evaluate b arg)))))

(bld-gen:defmeth2 * ((a funcl-function) (b number))
  (make-instance 'combination-function 
                 :combination-operation '*
                 :function-1 a
                 :function-2 b
                 :range (range a)
                 :domain (domain a)
                 :differentiator (lambda () (* (differentiate a) b))
                 :lambda-function (lambda (arg) (* (evaluate a arg) b))))

(bld-gen:defmeth2 * ((a number) (b funcl-function)) (* b a))

(bld-gen:defmeth2 / ((a funcl-function) (b funcl-function))
  (make-instance 'combination-function 
                 :combination-operation '/
                 :function-1 a 
                 :function-2 b
                 :range (range a)
                 :domain (domain a)
                 :differentiator (lambda () (/ (- (* (differentiate a) b) 
                                                  (* a (differentiate b))) 
                                               (* b b)))
                 :lambda-function (lambda (arg) (/ (evaluate a arg) (evaluate b arg)))))

(bld-gen:defmeth2 / ((a number) (b magicl:matrix))
  (* a (/ b)))

(bld-gen:defmeth2 / ((a number) (b funcl-function))
  (make-instance 'combination-function 
                 :combination-operation '/
                 :function-1 a 
                 :function-2 b
                 :range (range b)
                 :domain (domain b)
                 :differentiator (lambda () (* (- a) 
                                               (/ (differentiate b)
                                                  (* b b))))
                 :lambda-function (lambda (arg) (/ a (evaluate b arg)))))

(bld-gen:defmeth1 / ((a funcl-function)) (/ 1 a))

@export
(defgeneric compose (a b))
(defmethod compose ((a funcl-function) (b funcl-function))
  (make-instance 'combination-function 
                 :combination-operation 'compose
                 :function-1 a 
                 :function-2 b
                 :range (range a)
                 :domain (domain b)
                 :differentiator ;(lambda () (dot-product (differentiate b) (compose (differentiate a) b)))
                 (lambda () (tensor-contract (tensor-product (differentiate b)
                                                             (compose (differentiate a) b))
                                             0 (+ 0 (length (range b)))))
                 :lambda-function (lambda (arg) (evaluate a (evaluate b arg)))))

(defclass named-function (funcl-function) 
  ((name :accessor name :initarg :name)))

@export
(defvar *sin* (make-instance 'named-function
                             :domain nil
                             :name "sin"
                             :range nil
                             :lambda-function (lambda (x)
                                                (if (numberp x) (sin x)
                                                    (sin (aref (aops:reshape x nil)))))
                             :differentiator (lambda () *cos*)))
@export
(defvar *cos* (make-instance 'named-function
                             :domain nil
                             :range nil
                             :name "cos"
                             :lambda-function (lambda (x)
                                                (if (numberp x) (cos x)
                                                    (cos (aref (aops:reshape x nil)))))
                             :differentiator (lambda () (- *sin*))))
@export
(defvar *tan* (make-instance 'named-function
                             :domain 'scalar
                             :range 'scalar
                             :name "tan"
                             :lambda-function #'tan
                             :differentiator (lambda () (/ 1 (* *cos* *cos*)))))
@export
(defvar *exp* (make-instance 'named-function
                             :domain 'scalar
                             :range 'scalar
                             :name "exp"
                             :lambda-function #'exp
                             :differentiator (lambda () *exp*)))

(defmethod bld-gen:exp ((arg funcl-function)) (compose *exp* arg))
(defmethod bld-gen:sin ((arg funcl-function)) (compose *sin* arg))
(defmethod bld-gen:cos ((arg funcl-function)) (compose *cos* arg))
(defmethod bld-gen:tan ((arg funcl-function)) (compose *tan* arg))

(defun ith-component (function i)
  (make-instance 'funcl-function
                 :domain (domain function)
                 :range 'scalar
                 :lambda-function (lambda (arg) (aref (evaluate function arg) i))
                 :differentiator (lambda () (ith-component (differentiate function) i))))



(bld-gen:defmeth2 + ((a multivariate-polynomial) (b multivariate-polynomial))
  (make-multivariate-polynomial (add-multivariate-polynomial-coefficients 
                                 (coefficients a) (coefficients b))))

(bld-gen:defmeth2 * ((a multivariate-polynomial) (b multivariate-polynomial))
  (make-multivariate-polynomial (multiply-multivariate-polynomial-coefficients 
                                 (coefficients a) (coefficients b))))

(bld-gen:defmeth2 * ((a multivariate-polynomial) (b number)) 
  (make-multivariate-polynomial (scalar-multiply-multivariate-polynomial-coefficients
                                 b (coefficients a))))

(bld-gen:defmeth2 * ((a number) (b multivariate-polynomial)) (* b a))

(bld-gen:defmeth1 - ((a multivariate-polynomial)) 
  (make-multivariate-polynomial 
   (scalar-multiply-multivariate-polynomial-coefficients -1 (coefficients a))))

(bld-gen:defmeth2 - ((a multivariate-polynomial) (b multivariate-polynomial)) (+ a (- b)))

(bld-gen:defmeth2 + ((a multivariate-polynomial) (b number))
  (+ a (constant b)))

(bld-gen:defmeth2 + ((a number) (b multivariate-polynomial))
  (+ (constant a) b))
(defmethod identity-matrix-like ((matrix magicl:matrix)) 
  (identity-matrix (magicl:matrix-cols matrix)))

(defmethod identity-matrix-like ((matrix array))
  (identity-matrix (array-dimension matrix 0)))

(bld-gen:defmeth2 + ((a number) (b simple-array))
  (if (array-dimensions b)
      (if (= 1 (length (array-dimensions b)))
          (map 'vector (lambda (arg) (+ a arg)) b)
          (+ b (* a (identity-matrix-like b))))
      (+ a (aref b))))

(bld-gen:defmeth2 + ((a simple-array) (b number))
  (+ b a))

(bld-gen:defmeth2 - ((a funcl-function) (b number))
  (- a (constant b)))

(bld-gen:defmeth2 - ((a number) (b funcl-function))
  (- (constant a) b))

(bld-gen:defmeth2 - ((a simple-array) (b number))
  (if (array-dimensions a)
      (map 'vector (lambda (arg) (- arg b)) a)
      (- (aref a) b)))

(bld-gen:defmeth2 - ((a number) (b simple-array))
  (map 'vector (lambda (arg) (- a arg)) b))

(bld-gen:defmeth2 * ((a funcl-function) (b magicl:matrix))
  (make-instance 'combination-function 
                 :combination-operation '*
                 :function-1 a
                 :function-2 b
                 :range (range a)
                 :domain (domain a)
                 :differentiator (lambda () (* (differentiate a) b))
                 :lambda-function (lambda (arg) (* (evaluate a arg) b))))

(bld-gen:defmeth2 * ((a magicl:matrix) (b funcl-function)) (* b a))


(bld-gen:defmeth2 + ((a funcl-function) (b magicl:matrix))
  (make-instance 'combination-function 
                 :combination-operation '+ 
                 :function-1 a
                 :range (range a)
                 :domain (domain a)
                 :function-2 b
                 :differentiator (lambda () (differentiate a))
                 :lambda-function (lambda (arg) (+ (evaluate a arg) b))))

(bld-gen:defmeth2 + ((a magicl:matrix) (b funcl-function)) (+ b a))

;(bld-gen:defmeth2 + ((a magicl:matrix) (b number)) (+ b (aref  (magicl::matrix-data a) 0)))
;(bld-gen:defmeth2 + ((a number) (b magicl:matrix)) (+ b a))

(bld-gen:defmeth2 + ((a array) (b array))
  (aops:each #'+ a b))

(bld-gen:defmeth2 - ((a array) (b array)) (aops:each #'- a b))

@export ; shadow this function using bld later
(defgeneric floor-funcl (x))

(defmethod floor-funcl ((x number)) (floor x))
(defmethod floor-funcl ((x simple-array)) (map 'vector #'floor x))
(defmethod floor-funcl ((x funcl-function))
  (make-instance 'funcl-function
                 :differentiator (lambda () (constant (aops:zeros (append
                                                                   (domain x)
                                                                   (range x))) :domain (domain x)))
                 :lambda-function (lambda (arg) (floor-funcl (evaluate x arg)))
                 :domain (domain x)
                 :range (range x)))

@export
(defgeneric eig (matrix))

(defmethod eig ((matrix magicl:matrix)) (magicl:eig matrix))
(defmethod eig ((matrix simple-array)) (magicl:eig (simple-array->magicl-matrix matrix)))

(defmethod bld-gen:sqrt ((function funcl-function))
  (make-instance 'funcl-function
                 :lambda-function (lambda (arg) (sqrt (evaluate function arg)))
                 :differentiator (lambda () (* 0.5 (differentiate function)
                                               (/ (sqrt function))))
                 :domain (domain function)
                 :range (range function)))

(defmethod bld-gen:sqrt ((array array))
  (unless (array-dimensions array)
    (sqrt (aref array))))

(defmethod bld-gen:expt ((function funcl-function) exponent)
  (make-instance 'funcl-function
                 :lambda-function (lambda (arg) (expt (flatten-scalar (evaluate function arg)) exponent))
                 :differentiator (lambda () (* exponent (differentiate function) (expt function (- exponent 1))))
                 :domain (domain function)
                 :range (range function)))

(defmethod bld-gen:expt ((array array) exponent)
  (unless (array-dimensions array)
    (expt (aref array) exponent)))

(bld-gen:defmeth2 max ((array array) (array2 array))
  (max (aref array) (aref array2)))

(bld-gen:defmeth2 max ((array number) (array2 array))
  (max array (aref array2)))

(bld-gen:defmeth2 max ((array array) (array2 number))
  (max (aref array) array2))

(bld-gen:defmeth2 min ((array array) (array2 array))
  (min (aref array) (aref array2)))
