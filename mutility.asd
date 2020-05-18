;;;; mutility.asd

(asdf:defsystem #:mutility
  :description "modula's utilities."
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.5"
  :depends-on (#:alexandria
               #:local-time)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "mutility")
               ;; swank-extensions.lisp is conditionally loaded at the end of mutility.lisp
               )
  :in-order-to ((test-op (test-op "mutility/tests"))))

(asdf:defsystem #:mutility/tests
  :name "mutility/tests"
  :author "modula t. <defaultxr@gmail.com>"
  :description "FiveAM-based test suite for mutility."
  :license "MIT"
  :depends-on (#:mutility
               #:fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "test"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:mutility-tests
                                                         :mutility/tests))))
