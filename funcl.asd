#|
  This file is a part of funcl project.
  Copyright (c) 2018 Selwyn Simsek (sgs16@ic.ac.uk)
|#

#|
  Author: Selwyn Simsek (sgs16@ic.ac.uk)
|#

(defsystem "funcl"
  :version "0.1.0"
  :author "Selwyn Simsek"
  :license "LLGPL"
  :depends-on ("magicl"
               "lparallel"
               "local-time"
               "named-readtables"
               "cl-annot"
               "bld-gen"
               "bld-ode"
               "eazy-gnuplot")
  :components ((:module "src"
                :components
                ((:file "funcl")
                 (:file "types")
                 (:file "matrix-routines" :depends-on ("funcl"))
                 (:file "polynomial" :depends-on ("funcl"))
                 (:file "ode" :depends-on ("funcl"))
                 (:file "plot")
                 (:file "generics" :depends-on ("funcl"))
                 (:file "readtable" :depends-on ("polynomial")))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "funcl-test"))))
