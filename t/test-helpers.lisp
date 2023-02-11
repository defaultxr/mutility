;;;; t/test-helpers.lisp - tests for the test helpers.

(in-package #:mutility/tests)

(in-suite mutility-tests)

(test org-header-line-p
  (is (equal (multiple-value-list (org-header-line-p "* cool header"))
             (list "cool header" 1)))
  (is (equal (multiple-value-list (org-header-line-p "*** bar baz"))
             (list "bar baz" 3))))

(test org-list-line-p
  (is (equal (multiple-value-list (org-list-line-p "- foo"))
             (list "foo" 1)))
  (is (equal (multiple-value-list (org-list-line-p "  - bar baz"))
             (list "bar baz" 3)))
  (is-false (org-list-line-p "")))

(test stream-extract-org-headers
  (is (equal (stream-extract-org-headers (make-string-input-stream "* foo
** bar
*** baz
- this
that
the other thing
** qux"))
             (list "foo" "bar" "baz" "qux"))))

(test stream-extract-org-lists
  (is (equal (stream-extract-org-lists (make-string-input-stream "* foo
** bar
*** baz
- this
  - that
    - the other thing
-not this
-not that
** qux"))
             (list "this" "that" "the other thing"))))

(test stream-extract-org-links
  (is (equal (stream-extract-org-links (make-string-input-stream "[link 1] [link 2]
* [[address][title]]
** [[another address][another title]]
- [[link 3]]"))
             (list "link 1" "link 2" "[address][title]" "[another address][another title]" "[link 3]"))))

(test docstring-linked-symbol-names
  (is (equalp (list "bar" "baz:qux")
              (docstring-linked-symbol-names "foo `bar' barb `baz:qux' temp"))))

(test symbol-all-docstrings
  (is (equalp (list "Get a list of all docstrings for SYMBOL.")
              (symbol-all-docstrings 'symbol-all-docstrings))))

(test docstring-broken-links
  (is (set-equal (list "fjdksfjsdkfj" "jfkdsjk")
                 (docstring-broken-links "foo `fjdksfjsdkfj' foo2 `symbol-all-docstrings' barb `jfkdsjk' temp `docstring-broken-links' temp2"
                                         :package 'mutility)
                 :test #'string=))
  (is (equalp (list "cl:foof")
              (docstring-broken-links "foo `cl:if' barb `cl:foof' temp"
                                      :package 'mutility
                                      :scan-external-packages t))))

(test system-missing-attributes)

(test package-undocumented-symbols
  (is (equalp (list "BAR")
              (let ((package (make-package "MUTILITY/TEMP-PACKAGE")))
                (unwind-protect
                     (let ((documented-symbol (intern "FOO" package))
                           (undocumented-symbol (intern "BAR" package)))
                       (export (list documented-symbol undocumented-symbol) package)
                       (setf (documentation documented-symbol 'variable) "TOTALLY 100% DOCUMENTED!!")
                       (mapcar #'symbol-name (package-undocumented-symbols package)))
                  (delete-package package))))))

(test package-symbol-conflicts
  (is (set-equal (list "FN" "RANDOM-COIN")
                 (let ((package (make-package "MUTILITY/TEMP-PACKAGE")))
                   (unwind-protect
                        (progn
                          (export (list (intern "FOO" package)
                                        (intern "FN" package)
                                        (intern "RANDOM-COIN" package)
                                        (intern "BAR" package))
                                  package)
                          (mapcar #'symbol-name (package-symbol-conflicts package 'mutility)))
                     (delete-package package)))
                 :test #'string=)))

(test package-docstrings-with-broken-links
  (is (set-equal (list (list "FOO" "fjjj")
                       (list "BAR" "fjjjk")
                       (list "BAZ" "nonexistent" "fool"))
                 (let ((package (make-package "MUTILITY/TEMP-PACKAGE")))
                   (unwind-protect
                        (let ((foo (intern "FOO" package))
                              (bar (intern "BAR" package))
                              (baz (intern "BAZ" package)))
                          (export (list foo bar baz) package)
                          (setf (documentation foo 'variable) "foo `fjjj' bar"
                                (documentation bar 'variable) "bar `fjjjk' bar"
                                (documentation baz 'variable) "baz `mutility:random-coin' bar `nonexistent' `fool'y cooly")
                          (mapcar (lambda (res)
                                    (cons (symbol-name (car res))
                                          (cdr res)))
                                  (package-docstrings-with-broken-links package :scan-external-packages t)))
                     (delete-package package)))
                 :test (lambda (item-1 item-2)
                         (and (string= (car item-1) (car item-2))
                              (set-equal (cdr item-1) (cdr item-2) :test #'string=))))))
