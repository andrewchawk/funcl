(defpackage funcl
  (:use :cl)
  (:shadowing-import-from :bld-gen
                          + - * / expt
                          sin cos tan
                          atan asin acos
                          sinh cosh tanh
                          asinh acosh atanh
                          log exp sqrt abs
                          min max signum))

(in-package :funcl)

(defclass funcl-function () 
  ((domain :initarg :domain :accessor domain)
   (range :initarg :range :accessor range)
   (lambda-function :accessor lambda-function :initarg :lambda-function)
   (differentiator :accessor differentiator :initarg :differentiator)))

(defgeneric evaluate (function argument))
(defmethod evaluate ((function funcl-function) argument)
  (when (slot-boundp function 'lambda-function)
    (funcall (slot-value function 'lambda-function) argument)))

(defgeneric differentiate (function))
(defmethod differentiate ((function funcl-function)) (funcall (differentiator function)))

(defgeneric name (function))
(defmethod name ((function funcl-function)) "f")

(defmethod print-object ((object funcl-function) stream)
  (with-slots (domain range) object
    ;(format stream "domain: ~d, range: ~d~%" domain range)
    (print-unreadable-object (object stream)
      (format stream "~d: ~A -> ~A" (name object) (domain object) (range object)))))

(defun restrict-domain (function domain)
  (setf (domain function) domain)
  function)
