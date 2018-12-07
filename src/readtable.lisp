(in-package :funcl)
(annot:enable-annot-syntax)

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
(defvar *print-human-readable* nil)

(defmethod print-object ((object multivariate-polynomial) stream)
  (if *print-human-readable* 
      (with-slots (coefficients) object
        (format stream "~{~a~^ + ~}"
                (remove-if (lambda (str) (string= "" str))
                           (multidimensional-loop
                                (coefficients place)
                                collect (format nil "~[~:;~,2f ~{~{~[~*~*~;~,2f~*~:;~,2f^~s~]~}~}~]" 
                                                (if (zerop (apply #'aref coefficients place))
                                                    0 1)
                                                (apply #'aref coefficients place) 
                                                (mapcar #'list place '(x y z w a b c d e f g h) place))))))
      (if (typep (coefficients object) 'sequence)
          (format stream "#Q(~{~s~^ ~})" (coerce (coefficients object) 'list))
          (format stream "~A" (substitute #\Q #\A (format nil "~s" (coefficients object)))))))

(defsyntax syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\q #'|#q-reader|)
  (:case :upcase))

@export
(defun merge-readtable ()
  "Convenience function to merge the funcl custom readtable into *readtable*."
  (named-readtables:merge-readtables-into  *readtable* funcl::syntax))
