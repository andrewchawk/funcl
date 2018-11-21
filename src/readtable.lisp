(in-package :funcl)
(annot:enable-annot-syntax)

(defreadtable funcl::syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\q #'|#q-reader|)
  (:case :upcase))

