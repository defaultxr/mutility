(in-package #:mutility)

(defpackage #:mutility/tests
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

(test a
  "Test the `a' macro and its helper functions"
  (is (equal (mutility::repeat-by-! '(1))
             '(1)))
  (is (equal (mutility::repeat-by-! '(1!3))
             '(1 1 1)))
  (is (equal (mutility::repeat-by-! '(1!3!2))
             '((1 1 1) (1 1 1))))
  (is (equal (mutility::repeat-by-! '(1!3 !2))
             '((1 1 1) (1 1 1))))
  (is (equal (mutility::repeat-by-! '((* 2 3)!2))
             '((* 2 3) (* 2 3))))
  (is (equal (mutility::repeat-by-! '(1!(* 2 2)))
             '(1 1 1 1)))
  (is (equal (mutility::repeat-by-! '((* 2 1)!(* 2 2)))
             '((* 2 1) (* 2 1) (* 2 1) (* 2 1))))
  (is (equal (mutility::repeat-by-! '(1 (pn 1)))
             '(1 (pn 1))))
  (is (equal (mutility::repeat-by-! '(1 (pn 1) 3!2 2!3))
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

(test dolist*
  ;; FIX
  )

(test accumulating
  "Test the `accumulating' macro"
  (is (equal (list 1 2 3)
             (mutility::accumulating
               (mutility::accumulate 1)
               (mutility::accumulate 2)
               (mutility::accumulate 3))))
  (is (equal (list 0 2 4 6 8)
             (mutility::accumulating
               (dotimes (n 5)
                 (mutility::accumulate (* 2 n)))))))

(test define-obsolete-function-alias
  ;; FIX
  )

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

(test concat
  ;; FIX
  )

(test output
  ;; FIX
  )

(test split-string
  "Test split-string"
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

(test string-boolean
  ;; FIX
  )

(test my-intern
  ;; FIX
  )

(test un-intern
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

(test round-by
  "Test the `round-by' function"
  (is (= 3.5
         (round-by 3.26 0.5))
      "round-by gives incorrect results")
  (is (= 3.25
         (round-by 3.25 0.25))
      "round-by gives incorrect results when its input is a multiple of the divisor"))

(test round-by-direction
  "Test the `round-by-direction' function"
  (is-true (= 2.04
              (round-by-direction 2.03 0.02))
           "round-by-direction gives incorrect results for positive numbers")
  (is-true (= -2.02
              (round-by-direction -2.03 0.02))
           "round-by-direction gives incorrect results for negative numbers")
  (is-true (= 8
              (round-by-direction 5 4))
           "round-by-direction gives incorrect results for arguments 5, 4")
  (is-true (= 3.5
              (round-by-direction 3.9 -0.5))
           "round-by-drection gives incorrect results when rounding down")
  (is-true (= (+ 2 1/10)
              (round-by-direction (+ 2 19/100) -1/10))
           "round-by-direction gives incorrect results for rounding down ratios"))

(test length-upto
  ;; FIX
  )

(test nth-wrap
  ;; FIX
  )

(test elt-wrap
  "Test the `elt-wrap' function"
  (is (= 3
         (elt-wrap (list 0 1 2 3) 7))
      "elt-wrap returns incorrect results"))

(test find-any
  ;; FIX
  )

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
