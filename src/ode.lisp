(in-package :funcl)
(annot:enable-annot-syntax)

(defclass ode-solution (funcl-function)
  ((t0 :accessor t0 :initarg :t0)
   (range :accessor range :initform 'scalar)
   (ode-lambda :accessor ode-lambda :initarg :ode-lambda)))

@export
(defun integrate-first-order-ode-system (initial-state ode-lambda &key (t0 0) (differentiator (lambda () (error "Not implemented yet"))) (tol 1.0d-10))
  (make-instance 'ode-solution
                 :t0 t0
                 :ode-lambda ode-lambda
                 :domain 'square-matrix
                 :differentiator differentiator
                 :lambda-function (lambda (arg) 
                                    (let ((result (cadar (last (bld-ode:rka ode-lambda t0 arg initial-state
                                                                            :tol tol)))))
                                      result))))

@export
(defun integrate-second-order-ode (initial-state second-derivative &key (t0 0))
  (let ((system (integrate-first-order-ode-system initial-state
                                                  (lambda (time state &optional p)
                                                    (vector (aref state 1) (funcall second-derivative time state p)))
                                                  :t0 t0)))
    (values (ith-component system 0) (ith-component system 1))))

@export
(defun integrate-nth-order-ode (initial-state nth-derivative order &key (t0 0))
  (let ((system (integrate-first-order-ode-system initial-state
                                                  (lambda (time state &optional p)
                                                    (apply #'vector 
                                                           (append (mapcar (alexandria:curry #'aref state)
                                                                           (alexandria:iota (1- order) :start 1))
                                                                   (funcall nth-derivative time state p))))
                                                  :t0 t0)))
    (apply #'values (mapcar (alexandria:curry #'ith-component system) (alexandria:iota order)))))

