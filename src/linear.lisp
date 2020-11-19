(in-package :funcl)
(annot:enable-annot-syntax)

(defmethod solve-linear-system ((a array) (b array))
  (lla:solve a b))

@export
(defun matrix-rank (m &key (precision 1e-7))
  (let ((svd-diagonal
          (cl-num-utils.matrix:diagonal-matrix-elements
           (lla:svd-d
            (lla:svd m)))))
    (loop for v across svd-diagonal
          counting (> (abs v) precision))))

@export
(defun full-rank-p (m &key (precision 1e-7))
  (= (matrix-rank m :precision precision)
     (apply #'min (array-dimensions m))))

@export
(defun solve-linear-system-in-least-squares (a b &key (precision 1e-7))
  "Solves the system Ax=b in least squares. Returns the solution, the sum of squared residuals, and the norm of the solution vector. Uses the SVD of A. This will return the least norm solution if the solution set is not unique."
  (let* ((m (array-dimension a 0))
         (n (array-dimension a 1))
         (svd (lla:svd a :all))
         (u (lla:svd-u svd))
         (d
           (cl-num-utils.matrix:diagonal-matrix-elements
            (lla:svd-d svd)))
         (v (transpose (lla:svd-vt svd)))
         (d-pseudoinverse (diagonal-matrix
                           (map 'vector
                                (lambda (entry)
                                  (if (< (abs entry) precision)
                                      0
                                      (/ entry)))
                                d)
                           n m))
         (solution (* v d-pseudoinverse (transpose u) b)))
    (values solution (lla:nrm2 (- b (* a solution))) (lla:nrm2 solution))))

@export
(defun solve-linear-system-in-least-norm (a b &key (precision 1e-7))
  (solve-linear-system-in-least-squares a b :precision precision))

@export
(defun solution-exists-p (a b &key (precision 1e-7))
  "Returns the minimum norm solution to the system Ax=b if one exists, otherwise NIL."
  (multiple-value-bind (solution residual-sum-squared)
      (solve-linear-system-in-least-squares a b)
    (when (< residual-sum-squared precision)
      solution)))

(defun diagonal-matrix (diagonal m n)
  (let ((result (make-array (list m n) :initial-element 0)))
    (loop for i from 0 below (min m n)
          do (setf (aref result i i) (aref diagonal i))
          finally (return result))))
