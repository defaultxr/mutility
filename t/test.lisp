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
              "mutility's asdf system definition is missing attributes: ~s" missing)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :mutility)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s"
              undocumented)))

(test docstrings-broken-links
  "Check for any broken links in docstrings of exported symbols"
  (let ((symbols (docstrings-with-broken-links :mutility)))
    (is-false symbols
              "some exported symbols have docstrings that contain broken links: ~s" symbols)))

(test a
  "Test the `a' macro and its helper functions"
  (is (equal (m::repeat-by-! '(1))
             '(1)))
  (is (equal (m::repeat-by-! '(1!3))
             '(1 1 1)))
  (is (equal (m::repeat-by-! '(1!3!2))
             '((1 1 1) (1 1 1))))
  (is (equal (m::repeat-by-! '(1!3 !2))
             '((1 1 1) (1 1 1))))
  (is (equal (m::repeat-by-! '((* 2 3)!2))
             '((* 2 3) (* 2 3))))
  (is (equal (m::repeat-by-! '(1!(* 2 2)))
             '(1 1 1 1)))
  (is (equal (m::repeat-by-! '((* 2 1)!(* 2 2)))
             '((* 2 1) (* 2 1) (* 2 1) (* 2 1))))
  (is (equal (m::repeat-by-! '(1 (pn 1)))
             '(1 (pn 1))))
  (is (equal (m::repeat-by-! '(1 (pn 1) 3!2 2!3))
             '(1 (pn 1) 3 3 2 2 2)))
  (is (equal (a 0..5)
             (list 0 1 2 3 4 5))
      "a gives incorrect results for generated ranges")
  (is (equal (a 1 2!3 1)
             (list 1 2 2 2 1))
      "a gives incorrect results for repeated atoms")
  (is (equal (a 1 (list 2 3 4)!2 9)
             (list 1 (list 2 3 4) (list 2 3 4) 9))
      "a gives incorrect results for repeated lists"))

(test fn
  "Test the `fn' macro"
  ;; FIX
  )

(test accumulating
  "Test the `accumulating' macro"
  (is (equal (list 1 2 3)
             (accumulating
               (accumulate 1)
               (accumulate 2)
               (accumulate 3))))
  (is (equal (list 0 2 4 6 8)
             (accumulating
               (dotimes (n 5)
                 (accumulate (* 2 n))))))
  (is (equal (list 5 6 6 6)
             (accumulating
               (accumulate 4)
               (reset-accumulation)
               (accumulate 5)
               (dotimes (n 3)
                 (accumulate 6))))
      "accumulating's reset-accumulation function doesn't reset the accumulated list"))

(test dprint
  "Test the `dprint' macro"
  (is (eql 0
           (search "(+ 2 2): 4; (+ 6 6): 12;"
                   (with-output-to-string (*standard-output*)
                     (dprint (+ 2 2) (+ 6 6)))))
      "dprint doesn't give the correct output")
  (is (eql 4
           (let ((*standard-output* (make-string-output-stream)))
             (dprint (+ 2 2))))
      "dprint doesn't return its last value"))

(test keys
  "Test `keys' and its various methods"
  (is (null (keys nil))
      "keys doesn't return NIL when its input is nil")
  (is (equal (list :foo :baz)
             (keys (list :foo :bar :baz :qux)))
      "keys doesn't work correctly for plists")
  (is (let ((hash (make-hash-table)))
        (setf (gethash :foo hash) 1
              (gethash :bar hash) 2
              (gethash :baz hash) 3
              (gethash :qux hash) 4)
        (let ((k (keys hash)))
          (mapcar (lambda (x) (member x k)) (list :foo :bar :baz :qux))))
      "keys doesn't work correctly for hashes"))

(test upcase-intern
  ;; FIX
  )

(test friendly-string
  "Test the `friendly-string' function"
  (is (string= "foo"
               (friendly-string "foo"))
      "friendly-symbol doesn't convert strings to keywords correctly")
  (is (string= "foos-bar-baz-and-qux"
               (friendly-string "foo's bar, baz, and qux"))))

(test friendly-symbol
  "Test the `friendly-symbol' function"
  (is (eql :foo
           (friendly-symbol "foo"))
      "friendly-symbol doesn't convert strings to keywords correctly")
  (is (eql :foos-bar-baz-and-qux
           (friendly-symbol "foo's bar, baz, and qux"))))

(test concat
  "Test the `concat' function"
  (is (string= "FOOBAR"
               (concat 'foo nil 'bar)))
  (is (string= "123"
               (concat 1 2 3)))
  (is (string= "foobar"
               (concat nil "foo" "bar" nil))))

(test output
  "Test the `output' function"
  (is (string= (format nil "FOOBAR~%")
               (with-output-to-string (*standard-output*)
                 (output 'foo nil 'bar))))
  (is (string= (format nil "123~%")
               (with-output-to-string (*standard-output*)
                 (output nil 1 2 3))))
  (is (string= (format nil "foobar~%")
               (with-output-to-string (*standard-output*)
                 (output "foo" "bar" nil)))))

(test vowel-char-p
  "Test the `vowel-char-p' function"
  (dolist (c (list #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
    (is-true (vowel-char-p c))))

(test split-string
  "Test the `split-string' function"
  (is (equal (list "" "")
             (split-string "d" :char-bag (list #\d) :include-empty t))
      "split-string is incorrect for strings consisting only of the divider and include-empty true")
  (is (null (split-string "d" :char-bag (list #\d) :include-empty nil))
      "split-string is incorrect for strings consisting only of the divider and include-empty false")
  (is (equal (list "this" "that" "the" "other" "thing")
             (split-string "this that
the other thing" :char-bag (list #\space #\newline)))
      "split-string char-bag argument does not work properly")
  (is (equal (list "foo" "bar=baz")
             (split-string "foo=bar=baz" :max-num 2 :char-bag #\=))
      "split-string max-num argument does not work properly")
  (is (equal (list "foo" "bar" "baz===qux=")
             (split-string "foo=bar==baz===qux=" :max-num 3 :char-bag #\= :include-empty nil))
      "split-string gives incorrect results when max-num and char-bag are provided and the string ends in a divider"))

(test replace-all
  ;; FIX
  )

(test parse-boolean
  "Test the `parse-boolean' function"
  (is-false (position t (mapcar #'parse-boolean (list "n" "N" "off" "0" "d" "f"))))
  (is-false (position nil (mapcar #'parse-boolean (list "y" "Y" "on" "1" "e" "t")))))

(test friendly-ratio-string
  "Test the `friendly-ratio-string' function"
  (is (string-equal "1 1/4" (friendly-ratio-string 5/4)))
  (is (string-equal "1+2/7" (friendly-ratio-string 9/7 "+"))
      "friendly-ratio-string doesn't use the separator argument correctly"))

(test friendly-duration-string
  "Test the `friendly-duration-string' function"
  (is (string-equal "5:00" (friendly-duration-string 300)))
  (is (string-equal "1:00:00" (friendly-duration-string 3600)))
  (is (string-equal "0:08" (friendly-duration-string 8))))

(test pretty-print-tree
  "Test the `pretty-print-tree' function"
  ;; FIX
  )

(test wrap
  "Test the `wrap' function"
  (is (= 3
         (wrap 3 0 4))
      "wrap incorrectly affects numbers within the given range")
  (is (= 3
         (wrap 7 0 4))
      "wrap doesn't wrap numbers outside the given range correctly")
  (is (= 3
         (wrap 3 1 4))
      "wrap doesn't wrap numbers within the given range correctly (when the bottom is non-zero)")
  (is (= -1
         (wrap 4 -1 4))
      "wrap doesn't wrap numbers within the given range correctly (when the bottom is non-zero)"))

(test fold
  "Test the `fold' function"
  (is (= 0
         (fold 2 -1 1)))
  (is (= 0.2
         (fold 0.2 -1 1)))
  (is (= 1
         (fold -1 0 1)))
  (is (= 5
         (fold 5 0 10)))
  (is (= 6
         (fold 8 0 7)))
  (is (= 1
         (fold 5 0 2)))
  (is (= 2
         (fold -6 0 4)))
  (is (= -6
         (fold -190 -8 7))))

(test floor-by
  "Test the `floor-by' function"
  (is (= 1.5
         (floor-by 1.6 0.5)))
  (is (= 0
         (floor-by 1.6 2))))

(test ceiling-by
  "Test the `ceiling-by' function"
  (is (= 0.4
         (ceiling-by 0.22 0.2)))
  (is (= 50
         (ceiling-by 27 25))))

(test round-by
  "Test the `round-by' function"
  (is (= 3.5
         (round-by 3.26 0.5))
      "round-by gives incorrect results")
  (is (= 3.25
         (round-by 3.25 0.25))
      "round-by gives incorrect results when its input is a multiple of the divisor"))

(test list-length-upto
  (let* ((rand (random 100))
         (res (list-length-upto (make-list 100) rand)))
    (is (= rand res)
        "list-length-upto gave incorrect results for a list of length 100 and a MAX of ~s; returned ~s" rand res)))

(test list-length>=
  (let* ((ll (random 100))
         (rand (random 100))
         (res (list-length>= (make-list ll) rand)))
    (is (eql (>= ll rand) res)
        "list-length>= gave incorrect results for a list of length ~s and an N of ~s; returned ~s" ll rand res)))

(test list-length>
  ;; not really needed since it just forwards to `list-length>=' anyway...
  )

(test nth-wrap
  ;; FIX
  )

(test elt-wrap
  "Test the `elt-wrap' function"
  (is (= 3
         (elt-wrap (list 0 1 2 3) 7))
      "elt-wrap returns incorrect results"))

(test find-if*
  "Test the `find-if*' function"
  (is (equal (list 4 4)
             (multiple-value-list (find-if* (lambda (x) (> x 3)) (iota 5))))
      "find-any returns incorrect results for lists")
  (is (equal (list 4 4)
             (multiple-value-list (find-if* (lambda (x) (> x 3)) (coerce (iota 5) 'vector))))
      "find-any returns incorrect results for vectors"))

(test find-any
  "Test the `find-any' function"
  ;; FIX
  )

(test most
  "Test the `most' function"
  (is (equal (list 1 2 3)
             (most #'> (list (list 1) (list 1 2 3) (list 1 2)) :key #'length))
      "most returns incorrect results"))

(test mapcar-with-index
  ;; FIX
  )

(test flatten-1
  "Test the `flatten-1' function"
  (is (equal (list 1 2 (list 3 4) 5)
             (flatten-1 (list 1 (list 2 (list 3 4) 5))))
      "flatten-1 returns incorrect results"))

(test subseq*
  ;; FIX
  )

(test repeat
  ;; FIX
  )

(test split-sequence
  "Test the `split-sequence' function"
  (is (equal (list (list 1 2) (list 2) (list 3))
             (split-sequence (list 1 2 :- 2 :- 3) :-))
      "split-sequence returns incorrect results"))

(test left-trim
  "Test the `left-trim' function"
  (is (equal (list 3 4 5)
             (left-trim (list 0 1 2) (list 2 1 0 3 4 5)))
      "left-trim returns incorrect results"))

(test affixnew
  "Test the `affixnew' function"
  (let (foo
        (num 0))
    (flet ((add-one ()
             (prog1 num
               (incf num))))
      (affixnew foo :foo)
      (affixnew foo (add-one))
      (affixnew foo :bar)
      (affixnew foo (add-one))
      (is (equal (list :foo 0 :bar 1)
                 foo)
          "affixnew does not produce correct results"))))

(test insert-if
  "Test the `insert-if' function"
  (is (equal (list -2 -1 0 1 2)
             (insert-if #'plusp (list -2 -1 1 2) 0))
      "insert-if doesn't insert to the correct location")
  (is (equal (list 0)
             (insert-if #'plusp nil 0))
      "insert-if doesn't work if the input list is empty"))

(test insert-sorted
  "Test the `insert-sorted' function"
  (is (equal (list 1 2 3 4)
             (insert-sorted (list 1 3 4) 2))
      "insert-sorted doesn't insert in the middle of the list")
  (is (equal (list 1 2 3 4)
             (insert-sorted (list 1 2 3) 4))
      "insert-sorted doesn't insert to the end of the list")
  (is (equal (list 1 2 3 4)
             (insert-sorted (list 2 3 4) 1))
      "insert-sorted doesn't insert to the beginning of the list")
  (is (equal (list 1 2 2.5 3 4)
             (insert-sorted (list 1 2 3 4) 2.5))
      "insert-sorted doesn't insert floats correctly")
  (is (equal (list 0)
             (insert-sorted nil 0))
      "insert-sorted doesn't work if the input list is empty"))

(test random-coin
  "Test the `random-coin' function"
  (is (every (lambda (x) (eql t x))
             (loop :repeat 200 :collect (random-coin 1)))
      "random-coin returned a non-t value even though the PROBABILITY was 1")
  (is (every 'null
             (loop :repeat 200 :collect (random-coin 0)))
      "random-coin returned a non-nil value even though the PROBABILITY was 0"))

(test random-range
  "Test the `random-range' function"
  (let ((result (loop :repeat 200
                   :collect (random-range 10))))
    (is (every (lambda (x) (eql t x))
               (mapcar (lambda (x) (and (>= x 0)
                                        (<= x 10)))
                       result))
        "random-range did not return only items between 0 and LOW, inclusive, when only one argument was provided (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                   :collect (random-range 0 10))))
    (is (every (lambda (x) (eql t x))
               (mapcar (lambda (x) (and (>= x 0)
                                        (<= x 10)))
                       result))
        "random-range did not return only items between LOW and HIGH values, inclusive (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                   :collect (random-range 200.0))))
    (is (every (lambda (x) (typep x 'float))
               result)
        "random-range returned a non-float when its argument was a float (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                   :collect (random-range 0 1.0))))
    (is (every (lambda (x) (typep x 'float))
               result)
        "random-range returned a non-float even though one of its arguments was a float (result: ~a)"
        result)))

(test exponential-random-range
  "Test the `exponential-random-range' function"
  ;; FIX
  )

(test random-gauss
  ;; FIX
  )

(test save-hash-table
  ;; FIX
  )

(test restore-hash-table
  ;; FIX
  )

(test current-seconds
  ;; FIX
  )

(test function-arglist
  (is (equalp (list '&optional (list 'm::probability 0.5))
              (function-arglist 'random-coin))
      "function-arglist doesn't return correct results for `random-coin'")
  (is (equalp (list 'm::hash 'm::filename '&key (list 'm::if-exists :error))
              (function-arglist 'save-hash-table))
      "function-arglist doesn't return correct results for `save-hash-table'")
  (is (equalp (list '&rest 'm::objects)
              (function-arglist 'concat))
      "function-arglist doesn't return correct results for `concat'"))

(test lisp-connections
  ;; FIX
  )

(test open-url
  ;; FIX
  )

(test generate-temporary-file-name
  ;; FIX
  )

(test locate-dominating-file
  ;; FIX
  )
