(in-package :funcl)
(cl-annot:enable-annot-syntax)


@export
(defgeneric matrix= (mat1 mat2))

(defmethod matrix= ((mat1 simple-array) (mat2 simple-array))
  (every #'identity (map 'vector #'equalp mat1 mat2)))

(defmethod matrix= ((mat1 magicl:matrix) (mat2 magicl:matrix))
  (matrix= (magicl::matrix-data mat1) (magicl::matrix-data mat2)))

(defmethod matrix= ((mat1 simple-array) (mat2 magicl:matrix))
  (matrix= (magicl::matrix-data mat1) mat2))

(defmethod matrix= ((mat1 magicl:matrix) (mat2 simple-array)) (matrix= mat2 mat1))

@export
(defun list= (list1 list2)
  (equalp list1 list2))
