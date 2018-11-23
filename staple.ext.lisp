(in-package :staple)

(defclass my-page (staple:simple-page) ())


(defmethod staple:template ((system (eql (asdf:find-system :funcl))))
  (asdf:system-relative-pathname system #p"default.ctml"))

(defmethod staple:page-type ((system (eql (asdf:find-system :funcl))))
  'my-page)

(defmethod format-documentation ((docstring string) (page my-page))
  (flet ((replace-see (string start end mstart mend rstart rend)
           (declare (ignore start end))
           (let* ((match (subseq string (aref rstart 0) (aref rend 0)))
                  (identifier (plump:decode-entities match))
                  xref)
             (cond ((cl-ppcre:scan "^[-a-zA-Z]+://" identifier)
                    (format NIL "See <a href=\"~a\" class=\"exref\">~a</a>"
                            match match))
                   ((setf xref (xref identifier))
                    (format NIL "See <a href=\"~a\" class=\"xref\">~a</a>"
                            (plump:encode-entities xref) match))
                   (T
                    (subseq string mstart mend))))))
    (let* ((docstring (plump:encode-entities docstring))
           (docstring (cl-ppcre:regex-replace-all "[sS]ee (.*)" docstring #'replace-see)))
      (format NIL "~a" docstring))))
