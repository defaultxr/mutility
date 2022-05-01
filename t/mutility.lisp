;;;; t/mutility.lisp - tests for mutility's main functionalities.

(in-package #:mutility/tests)

(in-suite mutility-tests)

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
  (is-true (eql 2 (funcall (fn (+ _ 2)) 0))))

(test cut
  "Test the `cut' macro"
  (is-true (= 2 (funcall (cut '- 3 <>) 1)))
  (is-true (= 0 (funcall (cut '- 3 <> <>) 2 1)))
  (is-true (= 1 (funcall (cut '- 3 <> 0) 2)))
  (is-true (= 1 (funcall (cut <> 3 <>) '- 2))))

(test defclass+
  "Test the `defclass+' macro"
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
  "Test the `upcase-intern' function"
  (is (string= 'foo (upcase-intern "foo")))
  (is (eql (find-package 'mutility/tests)
           (symbol-package (upcase-intern "bar" 'mutility/tests)))))

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

(test string-designator-p
  "Test the `string-designator-p' function"
  (is-true (string-designator-p 'foo))
  (is-true (string-designator-p "FOO"))
  (is-true (string-designator-p :foo))
  (is-true (string-designator-p nil))
  (is-true (string-designator-p t))
  (is-false (string-designator-p 3))
  (is-false (string-designator-p (list 1 2)))
  (is-false (string-designator-p (lambda () "foo"))))

(test string-split
  "Test the `string-split' function"
  (is (equal (list "" "")
             (string-split "d" :char-bag (list #\d) :include-empty t))
      "string-split is incorrect for strings consisting only of the divider and include-empty true")
  (is (null (string-split "d" :char-bag (list #\d) :include-empty nil))
      "string-split is incorrect for strings consisting only of the divider and include-empty false")
  (is (equal (list "this" "that" "the" "other" "thing")
             (string-split "this that
the other thing" :char-bag (list #\space #\newline)))
      "string-split char-bag argument does not work properly")
  (is (equal (list "foo" "bar=baz")
             (string-split "foo=bar=baz" :max-num 2 :char-bag #\=))
      "string-split max-num argument does not work properly")
  (is (equal (list "foo" "bar" "baz===qux=")
             (string-split "foo=bar==baz===qux=" :max-num 3 :char-bag #\= :include-empty nil))
      "string-split gives incorrect results when max-num and char-bag are provided and the string ends in a divider"))

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

(test approx=
  "Test the `approx=' function"
  (is-true (approx= 0 0.1 0.1))
  (is-true (approx= 0 -0.1 0.1))
  (is-false (approx= 0 0.1 0.01)))

(test near-zero-p
  "Test the `near-zero-p' function"
  (is-true (near-zero-p 0.0001 0.0001))
  (is-true (near-zero-p -0.0001 0.0001))
  (is-true (near-zero-p 0.1 0.1)))

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
    (is (every #'floatp result)
        "random-range returned a non-float when its argument was a float (result: ~a)"
        result))
  (let ((result (loop :repeat 200
                      :collect (random-range 0 1.0))))
    (is (every #'floatp result)
        "random-range returned a non-float even though one of its arguments was a float (result: ~a)"
        result)))

(test exponential-random-range
  "Test the `exponential-random-range' function"
  ;; FIX
  )

(test random-gauss
  ;; FIX
  )

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
  (is (= 3
         (nth-wrap 7 (list 0 1 2 3)))
      "nth-wrap returns incorrect results for lists")
  (is (= 2
         (nth-wrap -2 (list 0 1 2 3)))
      "nth-wrap returns incorrect results for negative indexes")
  (is (= 2
         (nth-value 1 (nth-wrap 7 (list 0 1 2))))
      "nth-wrap's second return value is incorrect"))

(test elt-wrap
  "Test the `elt-wrap' function"
  (is (= 3
         (elt-wrap (list 0 1 2 3) 7))
      "elt-wrap returns incorrect results for lists")
  (is (= 3
         (elt-wrap (vector 0 1 2 3) 7))
      "elt-wrap returns incorrect results for vectors")
  (is (= 2
         (elt-wrap (list 0 1 2 3) -2))
      "elt-wrap returns incorrect results for negative indexes")
  (is (= 2
         (nth-value 1 (elt-wrap (list 0 1 2) 7)))
      "elt-wrap's second return value is incorrect"))

(test getf+
  "Test the `getf+' function"
  ;; FIX
  )

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

(test left-trim
  "Test the `left-trim' function"
  (is (equal (list 3 4 5)
             (left-trim (list 0 1 2) (list 2 1 0 3 4 5)))
      "left-trim returns incorrect results"))

(test sequence-split
  "Test the `sequence-split' function"
  (is (equal (list (list 1 2) (list 2) (list 3))
             (sequence-split (list 1 2 :- 2 :- 3) :-))
      "sequence-split returns incorrect results"))

(test sequence-replace
  "Test the `sequence-replace' function"
  (is (equal (list 0 1 :bar 2 3 :bar 4 5 :bar 6)
             (sequence-replace (list 0 1 :foo 2 3 :foo 4 5 :foo 6) :foo :bar))
      "sequence-replace returns incorrect results")
  (is (equal (list :bar 2 3 :bar 4 5 :foo 6)
             (sequence-replace (list :foo 2 3 :foo 4 5 :foo 6) :foo :bar :limit 2))
      "sequence-replace returns incorrect results when LIMIT is provided")
  (is (eql 3 (nth-value 1 (sequence-replace (list :foo 2 3 :foo 4 5 :foo 6) :foo :bar)))
      "sequence-replace doesn't return the number of replacements as its second value"))

(test insert-if
  "Test the `insert-if' function"
  (is (equal (list -2 -1 0 1 2)
             (insert-if #'plusp (list -2 -1 1 2) 0))
      "insert-if doesn't insert to the correct location")
  (is (equal (list 0)
             (insert-if #'plusp nil 0))
      "insert-if doesn't work if the input list is empty"))

(test function-designator
  "Test the `function-designator' type and `function-designator-p' function"
  (is-false (function-designator-p 0))
  (is-false (function-designator-p 'foo))
  (is-true (function-designator-p '+))
  (is-false (function-designator-p 'jfkdsjkf))
  (is-true (function-designator-p (lambda () 3))))

(test save-hash-table
  ;; FIX
  )

(test restore-hash-table
  ;; FIX
  )

(test all-classes
  "Test the `all-classes' function"
  ;; FIX
  )

(test subclasses-of
  "Test the `subclasses-of' function"
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

(test pathname-designator-p
  "Test the `pathname-designator-p' function"
  (is-true (pathname-designator-p "/home/blah.txt"))
  (is-true (pathname-designator-p #P"/home/blah.txt"))
  (is-false (pathname-designator-p nil))
  (is-false (pathname-designator-p '/home/blah.txt))
  (is-false (pathname-designator-p :foo)))

(test open-url
  ;; FIX
  )

(test generate-temporary-file-name
  ;; FIX
  )

(test locate-dominating-file
  ;; FIX
  )
