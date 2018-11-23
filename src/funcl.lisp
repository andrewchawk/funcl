(defpackage funcl
  (:use :cl :annot.class :cl-syntax)
  (:shadowing-import-from :bld-gen
                          + - * / expt
                          sin cos tan
                          atan asin acos
                          sinh cosh tanh
                          asinh acosh atanh
                          log exp sqrt abs
                          min max signum)
  (:export :syntax))

(in-package :funcl)
(annot:enable-annot-syntax)

@export
(defclass funcl-function () 
  ((domain :initarg :domain :accessor domain :documentation "S-expression that represents the domain of the function e.g. scalar, (matrix 3 3)")
   (range :initarg :range :accessor range :documentation "S-expression that represents the range of the function e.g. (vector 2), (square-matrix n)")
   (lambda-function :accessor lambda-function :initarg :lambda-function :documentation "Common Lisp function object that evaluates this function at a particular argument and returns the result.")
   (differentiator :accessor differentiator :initarg :differentiator :documentation ""))
  (:documentation "A general mathematical function from a specified domain to a range."))

@export
(defgeneric evaluate (function argument))
(defmethod evaluate ((function funcl-function) argument)
  (when (slot-boundp function 'lambda-function)
    (funcall (slot-value function 'lambda-function) argument)))

@export
(defgeneric differentiate (function))
(defmethod differentiate ((function funcl-function)) (funcall (differentiator function)))

@export
(defgeneric name (function)
  (:documentation "Returns the mathematician-friendly name of the function."))

(defmethod name ((function funcl-function)) "f")

(defmethod print-object ((object funcl-function) stream)
  (with-slots (domain range) object
    ;(format stream "domain: ~d, range: ~d~%" domain range)
    (print-unreadable-object (object stream)
      (format stream "~d: ~A -> ~A" (name object) (domain object) (range object)))))

@export
(defun restrict-domain! (function new-domain)
  "Setfs the domain slot of function to be new-domain and returns function."
  (setf (domain function) new-domain)
  function)
