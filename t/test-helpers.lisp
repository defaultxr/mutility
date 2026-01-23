;;;; t/test-helpers.lisp - tests for the test helpers.

(in-package #:mutility/tests)

(in-suite mutility-tests)

(test org-header-line-p
  "Test the `org-header-line-p' function"
  (is (equal (list "cool header" 1)
             (multiple-value-list (org-header-line-p "* cool header"))))
  (is (equal (list "bar baz" 3)
             (multiple-value-list (org-header-line-p "*** bar baz")))))

(test org-list-line-p
  "Test the `org-list-line-p' function"
  (is (equal (list "foo" 1)
             (multiple-value-list (org-list-line-p "- foo"))))
  (is (equal (list "bar baz" 3)
             (multiple-value-list (org-list-line-p "  - bar baz"))))
  (is-false (org-list-line-p "")))

(test stream-extract-org-headers
  "Test the `stream-extract-org-headers' function"
  (is (equal (list "foo" "bar" "baz" "qux")
             (stream-extract-org-headers (make-string-input-stream "* foo
** bar
*** baz
- this
that
the other thing
** qux")))))

(test stream-extract-org-header
  "Test the `stream-extract-org-header' function"
  (is (equal "bar"
             (stream-extract-org-header (make-string-input-stream "* foo
** bar
*** baz
- this
that
the other thing
** qux") "ba")))
  (is (equalp (list "baz"
                    "- this
that
the other thing")
              (multiple-value-list (stream-extract-org-header (make-string-input-stream "* foo
** bar
*** baz
- this
that
the other thing
** qux") "baz")))))

(test stream-extract-org-lists
  "Test the `stream-extract-org-lists' function"
  (is (equal (list "this" "that" "the other thing")
             (stream-extract-org-lists (make-string-input-stream "* foo
** bar
*** baz
- this
  - that
    - the other thing
-not this
-not that
** qux")))))

(test string-extract-org-links
  "Test the `string-extract-org-links' function"
  (is (equal (list "link 1" "link 2" (list "address" "title") (list "another address" "another title") (list "link 3"))
             (string-extract-org-links "[link 1] [link 2]
* [[address][title]]
** [[another address][another title]]
- [[link 3]]"))))

(test docstring-linked-symbol-names
  "Test the `docstring-linked-symbol-names' function"
  (is (equalp (list "bar" "baz:qux")
              (docstring-linked-symbol-names "foo `bar' barb `baz:qux' temp"))))

(test symbol-all-docstrings
  "Test the `symbol-all-docstrings' function"
  (is (equalp (list "Get a list of all docstrings for SYMBOL.")
              (symbol-all-docstrings 'symbol-all-docstrings))))

(test docstring-broken-links
  "Test the `docstring-broken-links' function"
  (is (set-equal (list "fjdksfjsdkfj" "jfkdsjk")
                 (docstring-broken-links "foo `fjdksfjsdkfj' foo2 `symbol-all-docstrings' barb `jfkdsjk' temp `docstring-broken-links' temp2"
                                         :package 'mutility)
                 :test #'string=))
  (is (equalp (list "cl:foof")
              (docstring-broken-links "foo `cl:if' barb `cl:foof' temp"
                                      :package 'mutility
                                      :scan-external-packages t))))

(test system-missing-attributes
  "Test the `system-missing-attributes' function"
  ;; FIX
  )

(test package-undocumented-symbols
  "Test the `package-undocumented-symbols' function"
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
  "Test the `package-symbol-conflicts' function"
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
  "Test the `package-docstrings-with-broken-links' function"
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
