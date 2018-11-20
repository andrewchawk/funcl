(in-package :funcl)

(defun square-matrix-p (matrix)
  (eq (magicl:matrix-cols matrix) (magicl:matrix-rows matrix)))

(defun matrix-size-p (matrix n m)
  ;(format t "n=~d, m=~d~%" n m)
  (and (or (eq n '*)
           (eq (magicl:matrix-rows matrix) n))
       (or (eq m '*)
           (eq (magicl:matrix-cols matrix) m))))

(defmacro deftypepred (name lambda-list &rest body)
  `(deftype ,name ,lambda-list
     (let ((predicate (gensym)))
       (setf (symbol-function predicate)
             (lambda (object) ,@body))
       `(satisfies ,predicate))))

(deftypepred matrix (&optional n m)
  (and (typep object 'magicl:matrix) (matrix-size-p object n m)))

(deftype square-matrix (&optional n) 
  `(and (matrix ,n ,n) (satisfies square-matrix-p)))

(deftype vektor (&optional n)
  `(matrix ,n 1))

(deftype scalar () '(or number (matrix 1 1)))

(deftypepred funxion (&optional domain range)
  (and (typep object 'funcl-function)
       (or (eq domain '*)
           (eq (domain object) domain))
       (or (eq range '*)
           (eq (range object) range))))
