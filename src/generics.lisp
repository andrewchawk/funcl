(in-package :funcl)
(cl-annot:enable-annot-syntax)

(bld-gen:defmeth2 + ((a magicl:matrix) (b magicl:matrix))
  (let ((n (magicl:matrix-rows a))
        (m (magicl:matrix-cols b))
        (unrolled-result (map 'vector '+ (magicl::matrix-data a) (magicl::matrix-data b))))
    (magicl::make-matrix :rows n :cols m :data (coerce unrolled-result `(simple-array ,(array-element-type (magicl::matrix-data a))
                                                                                      (,(* n m)))))))

@export
(defun simple-array->magicl-matrix (b) (magicl:make-complex-matrix (length b) 1 (coerce b 'list)))

(bld-gen:defmeth2 * ((a magicl:matrix) (b magicl:matrix)) (magicl:multiply-complex-matrices a b))
(bld-gen:defmeth2 * ((a magicl:matrix) (b number)) (magicl:scale b a))
(bld-gen:defmeth2 * ((a number) (b magicl:matrix)) (* b a))
(bld-gen:defmeth2 * ((a magicl:matrix) (b simple-array))
  (* a (simple-array->magicl-matrix b)))

(bld-gen:defmeth2 * ((a funcl-function) (b magicl:matrix)) (* a (constant b)))
(bld-gen:defmeth2 * ((a magicl:matrix) (b funcl-function)) (* (constant a) b))
(bld-gen:defmeth2 * ((a funcl-function) (b simple-array)) (* a (constant (simple-array->magicl-matrix b))))
(bld-gen:defmeth2 * ((a simple-array) (b funcl-function)) (* (constant (simple-array->magicl-matrix a)) b))


(bld-gen:defmeth1 - ((a magicl:matrix)) (* -1 a))
(bld-gen:defmeth2 - ((a magicl:matrix) (b magicl:matrix)) (+ a (- b)))
(bld-gen:defmeth1 / ((a magicl:matrix)) (magicl:inv a))

(defmethod bld-ode:norminfx ((a magicl:matrix)) (bld-ode:norminfx (magicl::matrix-data a)))

(defmethod expt ((a magicl:matrix) (b fixnum)) (magicl:exptm a b))

(defclass combination-function (funcl-function)
  ((combination-operation :accessor combination-operation :initarg :combination-operation)
   (function-1 :accessor function-1 :initarg :function-1)
   (function-2 :accessor function-2 :initarg :function-2)))

(defmethod name ((function combination-function))
  (let ((name-1 (name (function-1 function)))
        (name-2 (name (function-2 function))))
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
                 :differentiator (lambda () (* (differentiate b) (compose (differentiate a) b)))
                 :lambda-function (lambda (arg) (evaluate a (evaluate b arg)))))

(defclass named-function (funcl-function) 
  ((name :accessor name :initarg :name)))

@export
(defvar *sin* (make-instance 'named-function
                             :domain 'scalar
                             :name "sin"
                             :range 'scalar
                             :lambda-function #'sin
                             :differentiator (lambda () *cos*)))
@export
(defvar *cos* (make-instance 'named-function
                             :domain 'scalar
                             :range 'scalar
                             :name "cos"
                             :lambda-function #'cos
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

(bld-gen:defmeth2 - ((a multivariate-polynomial) (b number))
  (- a (constant b)))

(bld-gen:defmeth2 - ((a number) (b multivariate-polynomial))
  (- (constant a) b))


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

(bld-gen:defmeth2 + ((a magicl:matrix) (b number)) (+ b (aref  (magicl::matrix-data a) 0)))
(bld-gen:defmeth2 + ((a number) (b magicl:matrix)) (+ b a))
