;;;; mutility.asd - the mutility ASDF system definitions.

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
               #:local-time
               #:closer-mop
               (:feature :sbcl (:require :sb-introspect))) ; for function-arglist
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "mutility")
               (:file "ringbuffer")
               (:file "queue")
               ;; emacs-extensions.lisp is conditionally loaded at the end of mutility.lisp
               )
  :in-order-to ((test-op (test-op "mutility/tests"))))

(asdf:defsystem #:mutility/loopy
  :name "mutility: loopy"
  :description "modula's utilities: various additional looping constructs"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :depends-on (#:mutility
               #:trivial-do)
  :pathname "src/"
  :serial t
  :components ((:file "loopy")))

(asdf:defsystem #:mutility/files
  :name "mutility: file utilities"
  :description "modula's utilities: conveniences for working with files and directories"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :depends-on (#:mutility)
  :pathname "src/"
  :serial t
  :components ((:file "files")))

(asdf:defsystem #:mutility/most
  :name "mutility plus common subsystems"
  :description "modula's utilities plus the most common additional subsystems"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :depends-on (#:mutility
               #:mutility/loopy
               #:mutility/files))

(asdf:defsystem #:mutility/test-helpers
  :name "mutility: test helpers"
  :description "modula's utilities: helper functions useful when writing tests"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :depends-on (#:mutility)
  :pathname "src/"
  :serial t
  :components ((:file "test-helpers")))

(asdf:defsystem #:mutility/tests
  :name "mutility test suite"
  :author "modula t."
  :description "FiveAM-based test suite for mutility"
  :license "MIT"
  :depends-on (#:mutility
               #:mutility/most
               #:mutility/test-helpers
               #:fiveam
               ;; systems we check for symbol conflicts with
               #:alexandria
               #:cl-patterns)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "mutility")
               (:file "ringbuffer")
               (:file "queue")
               (:file "loopy")
               (:file "files")
               (:file "test-helpers"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:mutility-tests
                                                         :mutility/tests))))
