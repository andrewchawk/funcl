(in-package :funcl)
(annot:enable-annot-syntax)

(defsyntax syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\q #'|#q-reader|)
  (:case :upcase))

@export
(defun merge-readtable ()
  "Convenience function to merge the funcl custom readtable into *readtable*."
  (named-readtables:merge-readtables-into  *readtable* funcl::syntax))
