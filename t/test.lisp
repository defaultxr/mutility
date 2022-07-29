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

(def-fixture with-temporary-file (filename)
  "Make a temporary file at FILENAME for the duration of the body, deleting it afterward."
  (let ((filename (if (uiop:absolute-pathname-p filename)
                      filename
                      (concat (uiop:temporary-directory) filename))))
    (with-open-file (temporary-file-stream filename
                                           :direction :output :if-exists :rename)
      (&body)
      (delete-file temporary-file-stream))))

(test system-attributes
  "Check that the system has all the standard attributes"
  (let ((missing (system-missing-attributes :mutility)))
    (is-false missing
              "mutility's asdf system definition is missing attributes: ~s" missing)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :mutility)))
    (is-false undocumented
              "Some exported symbols do not have docstrings: ~s"
              undocumented)))

(test docstrings-broken-links
  "Check for any broken links in docstrings of exported symbols"
  (let ((symbols (docstrings-with-broken-links :mutility)))
    (is-false symbols
              "Some exported symbols have docstrings that contain broken links: ~s" symbols)))
