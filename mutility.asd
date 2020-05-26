;;;; mutility.asd

(asdf:defsystem #:mutility
  :name "mutility"
  :description "modula's utilities"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :homepage "https://github.com/defaultxr/mutility"
  :bug-tracker "https://github.com/defaultxr/mutility/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/mutility.git")
  :depends-on (#:alexandria
               #:local-time)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "mutility")
               ;; swank-extensions.lisp is conditionally loaded at the end of mutility.lisp
               )
  :in-order-to ((test-op (test-op "mutility/tests"))))

(asdf:defsystem #:mutility/test-helpers
  :description "modula's utilities with \"test helper\" functions"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :depends-on (#:mutility)
  :pathname "src/"
  :serial t
  :components ((:file "test-helpers")))

(asdf:defsystem #:mutility/tests
  :name "mutility tests"
  :author "modula t."
  :description "FiveAM-based test suite for mutility"
  :license "MIT"
  :depends-on (#:mutility
               #:mutility/test-helpers
               #:fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "test"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:mutility-tests
                                                         :mutility/tests))))
