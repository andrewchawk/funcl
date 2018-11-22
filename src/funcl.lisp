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
  ((domain :initarg :domain :accessor domain)
   (range :initarg :range :accessor range)
   (lambda-function :accessor lambda-function :initarg :lambda-function)
   (differentiator :accessor differentiator :initarg :differentiator)))

@export
(defgeneric evaluate (function argument))
(defmethod evaluate ((function funcl-function) argument)
  (when (slot-boundp function 'lambda-function)
    (funcall (slot-value function 'lambda-function) argument)))

@export
(defgeneric differentiate (function))
(defmethod differentiate ((function funcl-function)) (funcall (differentiator function)))

@export

(defgeneric name (function))
(defmethod name ((function funcl-function)) "f")

(defmethod print-object ((object funcl-function) stream)
  (with-slots (domain range) object
    ;(format stream "domain: ~d, range: ~d~%" domain range)
    (print-unreadable-object (object stream)
      (format stream "~d: ~A -> ~A" (name object) (domain object) (range object)))))

@export
(defun restrict-domain (function domain)
  (setf (domain function) domain)
  function)
