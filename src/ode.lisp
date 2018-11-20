(in-package :funcl)

(defclass ode-solution (funcl-function)
  ((t0 :accessor t0 :initarg :t0)
   (range :accessor range :initform 'scalar)
   (ode-lambda :accessor ode-lambda :initarg :ode-lambda)))

(defun integrate-first-order-ode-system (initial-state ode-lambda &key (t0 0) (differentiator (lambda () (error "Not implemented yet"))))
  (make-instance 'ode-solution
                 :t0 t0
                 :ode-lambda ode-lambda
                 :domain 'square-matrix
                 :differentiator differentiator
                 :lambda-function (lambda (arg) 
                                    (let ((result (cadar (last (bld-ode:rka ode-lambda t0 arg initial-state)))))
                                      result))))

(defun integrate-second-order-ode (initial-state second-derivative &key (t0 0))
  (let ((system (integrate-first-order-ode-system initial-state
                                                  (lambda (time state &optional p)
                                                    (vector (aref state 1) (funcall second-derivative time state p)))
                                                  :t0 t0)))
    (values (ith-component system 0) (ith-component system 1))))

(defun integrate-nth-order-ode (initial-state nth-derivative order &key (t0 0))
  (let ((system (integrate-first-order-ode-system initial-state
                                                  (lambda (time state &optional p)
                                                    (apply #'vector 
                                                           (append (mapcar (alexandria:curry #'aref state)
                                                                           (alexandria:iota (1- order) :start 1))
                                                                   (funcall nth-derivative time state p))))
                                                  :t0 t0)))
    (apply #'values (mapcar (alexandria:curry #'ith-component system) (alexandria:iota order)))))
#|
(defun solve-ermakov-equation (omega-t &key (initial-state #(1 0)) (omega-0 1) (t0 0))
  (integrate-second-order-ode initial-state 
                              (lambda (time state &optional p)
                                (declare (ignore p))
                                (let ((rho (aref state 0)))
                                  (- (/ (expt omega-0 2)
                                        (expt rho 3))
                                     (* rho (expt (evaluate omega-t time) 2))))) 
                              :t0 t))|
|#
