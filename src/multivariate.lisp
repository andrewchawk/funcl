(in-package :funcl)
(annot:enable-annot-syntax)

;; Experimental new tensor interface



@export
(defun perm-transpose (n a b) ; 0-index based for tensor ops
  (perm:perm-transpose-indexes (perm:perm-identity n) (1+ a) (1+ b)))

@export
(defun perm-cycle (n a b) ; 0-index based for tensor-ops
  (perm:list-to-perm
          (append (perm:perm-to-list
                   (perm:from-cycles
                    (list (apply #'perm:make-cycle (alexandria:iota (- (+ 2 b) (+ 1 a)) :start (1+ a))))))
                  (alexandria:iota (- n (- (+ 2 b) 1)) :start (1+ (- (+ 2 b) 1))))))
@export
(defun increment-permutation (permutation)
  (perm:list-to-perm (append '(1) (mapcar #'1+ (perm:perm-to-list permutation)))))

@export
(defgeneric tensor-permute (tensor permutation))

(defmethod tensor-permute ((tensor simple-array) (permutation perm:perm))
  (if (= 1 (cl-permutation:perm-size permutation))
      tensor
      (aops:permute (mapcar #'1- (perm:perm-to-list permutation)) tensor)))

(defmethod tensor-permute ((tensor funcl-function) (permutation perm:perm))
  (make-instance 'combination-function
                 :function-1 tensor
                 :range (when (range tensor) (perm:permute permutation (range tensor)))
                 :domain (domain tensor)
                 :combination-operation :permute
                 :lambda-function (lambda (arg) (tensor-permute (evaluate tensor arg) permutation))
                 :differentiator
                 (lambda ()
                   (tensor-permute (differentiate tensor) (increment-permutation permutation)))))

(defmethod tensor-permute ((tensor number) (permutation perm:perm)) tensor)

@export
(defgeneric tensor-product (a b))

(defmethod tensor-product ((a array) (b array))
  (aops:outer #'* a b))
(defmethod tensor-product ((a magicl:matrix) (b magicl:matrix))
  (aops:outer #'* (magicl-matrix->multidimensional-array a)
              (magicl-matrix->multidimensional-array b)))

(defmethod tensor-product ((a simple-array) (b number))
  (aops:outer #'* (make-array nil :initial-element b) a))
(defmethod tensor-product ((a number) (b simple-array)) (tensor-product b a))
(defmethod tensor-product ((a number) (b number)) (* a b))

(defmethod tensor-product ((a funcl-function) (b funcl-function))
  (make-instance 'combination-function
                 :combination-operation 'tensor-product
                 :function-1 a
                 :function-2 b
                 :range (append (range a) (range b))
                 :domain (domain a)
                 :differentiator (lambda () (+ (tensor-product (differentiate a) b)
                                               (tensor-permute 
                                                (tensor-product a (differentiate b))
                                                (differentiate-product-cycle (length (range a))
                                                                             (length (range b))))))
                 :lambda-function (lambda (arg) (tensor-product (evaluate a arg)
                                                                (evaluate b arg)))))

(defun tensor-contract-dimensions (tensor index-1 index-2 )
  (%tensor-contract-dimensions (array-dimensions tensor) index-1 index-2))

(defun %tensor-contract-dimensions (dimensions index-1 index-2 )
  (if (eq dimensions 'scalar)
      nil
      (when dimensions
        (append
         (subseq dimensions 0 (min index-1 index-2))
         (subseq dimensions (1+ (min index-1 index-2)) (max index-1 index-2))
         (subseq dimensions (1+ (max index-1 index-2)))))))

@export
(defgeneric tensor-contract (tensor index-1 index-2))

(defmethod tensor-contract ((tensor simple-array) (index-1 integer) (index-2 integer))
  (let* ((dimensions (array-dimensions tensor))
         (new-dimensions (tensor-contract-dimensions tensor index-1 index-2)))
    (if dimensions
        (progn
          (assert (= (nth index-1 dimensions) (nth index-2 dimensions)))
          (aops:generate (lambda (subscript)
                           (loop for i from 0 below (nth index-1 dimensions)
                                 summing (apply #'aref tensor
                                                (append (subseq subscript 0 (min index-1 index-2))
                                                        (list i)
                                                        (subseq subscript (min index-1 index-2)
                                                                (1- (max index-1 index-2)))
                                                        (list i)
                                                        (subseq subscript (1- (max index-1 index-2)))))))
                         new-dimensions :subscripts))
        tensor)))

(defmethod tensor-contract ((tensor funcl-function) (index-1 integer) (index-2 integer))
  (make-instance 'combination-function
                 :combination-operation 'tensor-contract
                 :function-1 tensor
                 :range (%tensor-contract-dimensions (range tensor) index-1 index-2)
                 :domain (domain tensor)
                 :differentiator (lambda () (tensor-contract (differentiate tensor) (1+ index-1) (1+ index-2)))
                                        ; increment the contraction index as differentation comes at the start
                 :lambda-function (lambda (arg) (tensor-contract (evaluate tensor arg) index-1 index-2))))

(defmethod tensor-contract ((tensor number) (index-1 integer) (index-2 integer)) tensor)


(defun differentiate-product-cycle (n m)
  "Implementation taken from 'Product permutations'"
  (apply #'perm:make-perm (append (list (+ n 1))
                                  (alexandria:iota n :start 1)
                                  (alexandria:iota m :start (+ n 2)))))
