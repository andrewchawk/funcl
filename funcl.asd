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
               "staple" ; for documentation
               "local-time"
               "cl-syntax"
               "cl-annot"
               "bld-gen"
               "bld-ode"
               "eazy-gnuplot")
  :source-control (:git "https://github.com/selwynsimsek/funcl.git")
  :components ((:module "src"
                :components
                ((:file "funcl")
                 (:file "types")
                 (:file "sequence-utils")
                 (:file "matrix-routines" :depends-on ("funcl"))
                 (:file "polynomial" :depends-on ("funcl" "sequence-utils"))
                 (:file "ode" :depends-on ("funcl"))
                 (:file "plot")
                 (:file "generics" :depends-on ("funcl"))
                 (:file "readtable" :depends-on ("polynomial")))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "funcl-test"))))
