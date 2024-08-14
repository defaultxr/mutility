;;;; t/test.lisp - basic tests and test fixtures/utilities for the mutility test suite.

(in-package #:mutility)

(defpackage #:mutility/tests
  (:local-nicknames (:m :mutility))
  (:use #:cl
        #:alexandria
        #:mutility
        #:fiveam))

(in-package #:mutility/tests)

(def-suite mutility-tests
  :description "mutility test suite.")

(in-suite mutility-tests)

(test system-attributes
  "Check that the system has all the standard attributes"
  (let ((missing (system-missing-attributes :mutility)))
    (is-false missing
              "mutility's asdf system definition is missing attributes: ~S" missing)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (package-undocumented-symbols :mutility)))
    (is-false undocumented
              "Some exported symbols do not have docstrings: ~S" undocumented)))

(test docstrings-broken-links
  "Check for any broken links in docstrings of exported symbols"
  (let ((symbols (package-docstrings-with-broken-links :mutility)))
    (is-false symbols
              "Some exported symbols have docstrings that contain broken links: ~S" symbols)))

(test symbol-conflicts
  "Check for symbol conflicts with a few other libraries"
  (let ((conflicts (package-symbol-conflicts 'mutility
                                             ;; check against thundersnow only if it is already loaded
                                             ;; once thundersnow is on quicklisp we can probably just always check it
                                             #+#.(cl:if (cl:find-package "THUNDERSNOW") '(:and) '(:or)) 'thundersnow
                                             ;; FIX: check against serapeum as well in the future; right now we conflict with its split-sequence and concat functions
                                             'alexandria 'cl-patterns)))
    (is-false conflicts
              "Symbols exported by mutility conflict with symbols from other systems: ~S" conflicts)))
