(in-package :funcl)
(annot:enable-annot-syntax)

(defun ensure-length (array n &key (initial-element 0))
  (if (< (length array) n)
      (let ((result (make-array (list n) :initial-element initial-element)))
        (loop for i from 0 below (length array) do 
             (setf (aref result i) (aref array i)) finally (return result)))
      array))

(defun ensure-list-length (list n &key (initial-element 0))
  (loop for i from 0 below (max n (length list))
       collecting (if (< i (length list)) 
                      (nth i list)
                      initial-element)))

(defun ensure-list-length-of (list destination &key (initial-element 0))
  (ensure-list-length list (length destination) :initial-element initial-element))

(defun ensure-length-of (array target &key (initial-element 0)) 
  (ensure-length array (length target) :initial-element initial-element))

(defun increment (ref coefficients-dimensions)
  (let ((new-ref (loop for i from 0 below (length ref) collecting
                      (if (eq i (1- (length ref)))
                          (1+ (nth i ref))
                          (nth i ref)))))
    (loop for i from (1- (length new-ref)) downto 0
       do (if (>= (nth i new-ref) (nth i coefficients-dimensions))
              (progn 
                (setf (nth i new-ref) 0)
                (if (zerop i)
                    (return nil)
                    (incf (nth (1- i) new-ref))))
              (return new-ref)))))

(defun jump-place (place index &optional (delta 1))
  (loop for r in place
       for i from 0 below (length place)
       collecting (if (eq i index)
                      (+ delta r)
                      r)))

(defun binary-and (v1 v2) (and v1 v2))
(defun all (&rest clauses) 
  (cond ((null clauses) t)
        ((eq 1 (length clauses)) (car clauses))
        (t (reduce #'binary-and clauses))))

(defun place-in-array-p (place array)
  "Returns T if and only if (apply array place) will succeed."
  (and
   (apply #'all (mapcar #'< place (array-dimensions array)))
   (apply #'all (mapcar (lambda (x) (>= x 0)) place))))

(defun place-within-array-p (place array)
  "Returns T if (apply array new-place) will succeed, where new-place is obtained by deleting zeroes off the end of place."
  (and (place-in-array-p place array)
       (apply #'all (mapcar #'zerop (nthcdr (array-rank array) place)))))

(defun array-map (function &rest arrays)
  "maps the function over the arrays.
   Assumes that all arrays are of the same dimensions.
   Returns a new result array of the same dimension."
  (flet ((make-displaced-array (array)
           (make-array (reduce #'* (array-dimensions array))
                       :displaced-to array)))
    (let* ((displaced-arrays (mapcar #'make-displaced-array arrays))
           (result-array (make-array (array-dimensions (first arrays))))
           (displaced-result-array (make-displaced-array result-array)))
      (declare (dynamic-extent displaced-arrays displaced-result-array))
      (apply #'map-into displaced-result-array function displaced-arrays)
      result-array)))

(defmacro multidimensional-loop ((array place &optional name)  &rest body)
  (let ((name-symbol (if name name (gensym "name")))
        (place-symbol (if place place (gensym "place"))))
    `(let ((,name-symbol ,array))
       (loop with ,place-symbol = (make-list (array-rank ,name-symbol) :initial-element 0)
          while ,place-symbol
            ,@body
          do (setf ,place-symbol (increment ,place-symbol (array-dimensions ,name-symbol)))))))
