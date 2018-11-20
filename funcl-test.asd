#|
  This file is a part of funcl project.
  Copyright (c) 2018 Selwyn Simsek (sgs16@ic.ac.uk)
|#

(defsystem "funcl-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Selwyn Simsek"
  :license "LLGPL"
  :depends-on ("funcl"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "funcl"))))
  :description "Test system for funcl"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
