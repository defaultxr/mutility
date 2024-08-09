;;;; t/mutility.lisp - tests for mutility's main functionalities.

(in-package #:mutility/tests)

(in-suite mutility-tests)

(test expand-ranges
  "Test the `mutility::expand-ranges' function"
  (is (equal (list 0 1 2 3 4 5 -2 -1 0 1 2)
             (mutility::expand-ranges '(0..5 -2..2))))
  (is (equal (list 1 2 3 4 -9 -10 -11)
             (mutility::expand-ranges '(1..4 -9..-11)))))

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
  (is-true (eql 2 (funcall (fn (+ _ 2)) 0)))
  (is-true (eql 1/2 (funcall (fn (/ _0 _1)) 1 2)))
  (is-true (string= "howya" (funcall (fn (concat _1 _3)) "hey" "how" "are" "ya")))
  (is-true (string= "hihi2hihi" (funcall (fn (concat _ _0 "2" _0 _)) "hi"))))

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

(test +whitespace-chars+
  "Test the `+whitespace-chars+' constant"
  (is (member #\space +whitespace-chars+ :test #'char=)))

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
    (is-true (vowel-char-p c)))
  (is-true (vowel-char-p #\y :include-y t))
  (is-true (vowel-char-p #\w :include-w t)))

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
             (string-split "foo=bar=baz" :count 2 :char-bag #\=))
      "string-split count argument does not work properly")
  (is (equal (list "foo" "bar" "baz===qux=")
             (string-split "foo=bar==baz===qux=" :count 3 :char-bag #\= :include-empty nil))
      "string-split gives incorrect results when count and char-bag are provided and the string ends in a divider"))

(test string-split-by-string
  "Test the `string-split-by-string' function"
  (is (equal (list "this" "that" "the-other thing")
             (string-split-by-string "this - that - the-other thing" " - "))
      "string-split-by-string returns incorrect results")
  (is (equal (list "this" "that" "the-other thing - the other other thing")
             (string-split-by-string "this - that - the-other thing - the other other thing" " - " :count 3))
      "string-split-by-string with the count argument returns incorrect results")
  (is (equal (list "this" "that" "the" "" "other thing")
             (string-split-by-string "this - that - the -  - other thing" " - " :include-empty t))
      "string-split-by-string with the include-empty argument returns incorrect results")
  (is (equal (list "this" "that" "the" " - other thing")
             (string-split-by-string "this - that - the -  - other thing" " - " :count 4 :include-empty t))
      "string-split-by-string with the count and include-empty arguments returns incorrect results")
  (is (equal (list "foo" "bar" "baz")
             (string-split-by-string "foodDbarDdbaz" "dd" :char-comparison #'char-equal))
      "string-split-by-string with 'char-equal as its char-comparison argument does not split case-insensitively"))

(test string-join*
  "Test the `string-join*' function"
  (is (string= (string-join* (list "foo" "bar" "baz") "-")
               "foo-bar-baz"))
  (is (string= (string-join* (list "foo" nil "baz") "-")
               "foo-baz"))
  (is (string= (string-join* (list "foo" nil "baz" nil nil "qux" nil) "$")
               "foo$baz$qux")))

(test replace-all
  "Test the `replace-all' function"
  ;; FIX
  )

(test parse-boolean
  "Test the `parse-boolean' function"
  (is-false (position t (mapcar #'parse-boolean (list "n" "N" "off" "0" "d" "f"))))
  (is-false (position nil (mapcar #'parse-boolean (list "y" "Y" "on" "1" "e" "t"))))
  (is-true (parse-boolean "blah" t))
  (is-false (parse-boolean "2" nil)))

(test ip-vector-string
  "Test the `ip-vector-string' function"
  (is (equal "127.0.0.1" (ip-vector-string #(127 0 0 1))))
  (is (equal "64.28.92.255" (ip-vector-string #(64 28 92 255))))
  (is (equal "99.98.97.96" (ip-vector-string "99.98.97.96"))))

(test ip-string-vector
  "Test the `ip-string-vector' function"
  (is (equalp #(127 0 0 1) (ip-string-vector "127.0.0.1")))
  (is (equalp #(64 28 92 255) (ip-string-vector "64.28.92.255")))
  (is (equalp #(99 98 97 96) (ip-string-vector #(99 98 97 96)))))

(test url-p
  "Test the `url-p' function"
  (is-true (url-p "http://w.struct.ws"))
  (is-true (url-p "https://struct.ws"))
  (is-true (url-p "http:/struct.ws"))
  (is-true (url-p "http://///w.struct.ws"))
  (is-false (url-p "http:w.struct.ws"))
  (is-false (url-p "www.struct.ws"))
  (is-false (url-p "struct.ws/"))
  (is-false (url-p "HTTPS://W.STRUCT.WS/"))
  (is-true (url-p "HTTPS://W.STRUCT.WS/" :ignore-case t)))

(test friendly-ratio-string
  "Test the `friendly-ratio-string' function"
  (is (string-equal "1 1/4" (friendly-ratio-string 5/4)))
  (is (string-equal "1+2/7" (friendly-ratio-string 9/7 "+"))
      "friendly-ratio-string doesn't use the separator argument correctly")
  (is (string-equal "-1 1/4" (friendly-ratio-string -5/4))
      "friendly-ratio-string doesn't handle negative ratios properly"))

(test friendly-duration-string
  "Test the `friendly-duration-string' function"
  (is (string-equal "5:00" (friendly-duration-string 300)))
  (is (string-equal "1:00:00" (friendly-duration-string 3600)))
  (is (string-equal "0:08" (friendly-duration-string 8))))

(test friendly-bytes
  "Test the `friendly-bytes' function"
  (is (equal (list 1 "Kilobyte") (friendly-bytes 1024)))
  (is (equal (list 375/128 "KB") (friendly-bytes 3000 :short t)))
  (is (equal (list 78125/16384 "MB") (friendly-bytes 5000000 :short t)))
  (is (equal (list 9765625/2097152 "GB") (friendly-bytes 5000000000 :short t)))
  (is (equal (list 1220703125/268435456 "TB") (friendly-bytes 5000000000000 :short t)))
  (is (equal (list 152587890625/34359738368 "PB") (friendly-bytes 5000000000000000 :short t))))

(test friendly-bytes-string
  "Test the `friendly-bytes-string' function"
  (is (string= "2.93 KB" (friendly-bytes-string 3000 :short t)))
  (is (string= "19.53 KB" (friendly-bytes-string 20000 :short t))))

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
  "Test the `random-gauss' function"
  ;; FIX
  )

(test nth-wrap
  "Test the `nth-wrap' function"
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
      "find-if* returns incorrect results for lists")
  (is (equal (list 4 4)
             (multiple-value-list (find-if* (lambda (x) (> x 3)) (coerce (iota 5) 'vector))))
      "find-if* returns incorrect results for vectors"))

(test find-member
  "Test the `find-member' function"
  (is (eql 3 (find-member (list 1 2 3) (list 3 4 5))))
  (is-false (find-member (list 1 2 3) (list 4 5 6))))

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

(test flop
  "Test the `flop' function"
  (is (equal '((0 2 1) (1 0 2) (2 1 0))
             (flop '((0 1 2) (2 0 1) (1 2 0))))
      "flop returns incorrect results"))

(test subseq*
  "Test the `subseq*' function"
  ;; FIX
  )

(test repeat
  "Test the `repeat' function"
  ;; FIX
  )

(test left-trim
  "Test the `left-trim' function"
  (is (equal (list 3 4 5)
             (left-trim (list 0 1 2) (list 2 1 0 3 4 5)))
      "left-trim returns incorrect results")
  (is (equal (list 3 2 4 5)
             (left-trim (list 0 1 2) (list 2 1 0 3 2 4 5)))
      "left-trim returns incorrect results")
  (is (equalp (vector 3 4 5)
              (left-trim (vector 0 1 2) (vector 2 1 0 3 4 5)))
      "left-trim doesn't work on vectors"))

(test list-left-trim
  "Test the `list-left-trim' function"
  (is (equal (list 3 4 5)
             (list-left-trim (list 0 1 2) (list 2 1 0 3 4 5)))
      "list-left-trim returns incorrect results")
  (is (equal (list 3 2 4 5)
             (list-left-trim (list 0 1 2) (list 2 1 0 3 2 4 5)))
      "list-left-trim returns incorrect results"))

(test sequence-split
  "Test the `sequence-split' function"
  (is (equal (list (list 1 2) (list 2) (list 3))
             (sequence-split (list 1 2 :- 2 :- 3) :-))
      "sequence-split returns incorrect results")
  (is (equal (list "foo" "ar" "az")
             (sequence-split "foo bar baz" #\space :offset 2))
      "sequence-split with OFFSET argument returns incorrect results")
  (is (equal (list "foo" "ar" "az qux")
             (sequence-split "foo bar baz qux" " b" :test #'search))
      "sequence-split with TEST argument returns incorrect results")
  (is (equal (list "foo" "bar" "baz")
             (sequence-split "foo - bar - baz" " - " :test #'search))
      "sequence-split with TEST argument returns incorrect results"))

(test sequence-replace
  "Test the `sequence-replace' function"
  (is (equal (list 0 1 :bar 2 3 :bar 4 5 :bar 6)
             (sequence-replace (list 0 1 :foo 2 3 :foo 4 5 :foo 6) :foo :bar))
      "sequence-replace returns incorrect results")
  (is (equal (list :bar 2 3 :bar 4 5 :foo 6)
             (sequence-replace (list :foo 2 3 :foo 4 5 :foo 6) :foo :bar :count 2))
      "sequence-replace returns incorrect results when COUNT is provided")
  (is (equal (list (list 1) :bar (list 3) :bar (list 2))
             (sequence-replace (list (list 1) (list :foo) (list 3) (list :foo) (list 2)) :foo :bar :key #'car))
      "sequence-replace returns incorrect results when KEY is provided")
  (is (eql 3 (nth-value 1 (sequence-replace (list :foo 2 3 :foo 4 5 :foo 6) :foo :bar)))
      "sequence-replace doesn't return the number of replacements as its second value"))

(test balanced-subsequences
  "Test the `balanced-subsequences' function"
  (is (equal (list "bar" "qux [this]")
             (balanced-subsequences "foo [bar] baz [qux [this]] that" :open #\[ :close #\] :test #'char=))))

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

(test mapshort
  "Test the `mapshort' function"
  (is (equal (list 11 22 33)
             (mapshort #'+ (list 10 20 30 40 50) (list 1 2 3)))))

(test mapwrap
  "Test the `mapwrap' function"
  (is (equal (list 11 22 33 41 52)
             (mapwrap #'+ (list 10 20 30 40 50) (list 1 2 3))))
  (is (equal (list 1 3 3)
             (mapwrap #'+ (list 0 1) (list 1) (list 0 1 2)))
      "mapwrap returns incorrect results")
  (is (equal (list 0 2 2 4 4 6)
             (mapwrap #'+ (list 0 1) (list 0 1 2 3 4 5)))
      "mapwrap doesn't wrap indexes of the shorter lists correctly"))

(test mapfold
  "Test the `mapfold' function"
  (is (equal (list 11 22 33 42 51)
             (mapfold #'+ (list 10 20 30 40 50) (list 1 2 3)))))

(test maptable
  "Test the `maptable' function"
  (is (equal (list (list 11 12 13) (list 21 22 23) (list 31 32 33) (list 41 42 43) (list 51 52 53))
             (maptable #'+ (list 10 20 30 40 50) (list 1 2 3)))))

(test mapcross
  "Test the `mapcross' function"
  (is (equal (list 11 12 13 21 22 23 31 32 33 41 42 43 51 52 53)
             (mapcross #'+ (list 10 20 30 40 50) (list 1 2 3))))
  (is (equal (list 0 1 2 3 4 5 6 7)
             (mapcross #'+ (list 0 4) (list 0 1 2 3))))
  (is (equal (list 11 12 13 14 12 13 14 15 12 13 14 15 13 14 15 16 13 14 15 16 14 15 16 17)
             (mapcross #'+ (list 1 2 3) (list 4 5) (list 6 7 8 9)))))

(test save-hash-table
  "Test the `save-hash-table' function"
  ;; FIX
  )

(test restore-hash-table
  "Test the `restore-hash-table' function"
  ;; FIX
  )

(test all-classes
  "Test the `all-classes' function"
  (is (member (find-class 'cl:pathname) (all-classes 'cl)))
  (is (member (find-class 'cl:array) (all-classes 'cl)))
  (is (member (find-class 'cl:number) (all-classes 'cl)))
  (is (member (find-class 'cl:symbol) (all-classes 'cl)))
  (is (member (find-class 'cl:character) (all-classes 'cl))))

(test subclasses-of
  "Test the `subclasses-of' function"
  ;; FIX
  )

(test find-class-slot
  "Test the `find-class-slot' function"
  ;; FIX
  )

(test lisp-uptime
  "Test the `lisp-uptime' function"
  (is-true (plusp (lisp-uptime))
           "lisp-uptime returned a value other than a positive number"))

(test function-arglist
  "Test the `function-arglist' function"
  (is (equalp (list '&optional (list 'm::probability 0.5))
              (function-arglist 'random-coin))
      "function-arglist doesn't return correct results for `random-coin'")
  (is (equalp (list 'm::hash 'm::filename '&key (list 'm::if-exists :error))
              (function-arglist 'save-hash-table))
      "function-arglist doesn't return correct results for `save-hash-table'")
  (is (equalp (list '&rest 'm::objects)
              (function-arglist 'concat))
      "function-arglist doesn't return correct results for `concat'"))

(test systems-depending-on
  "Test the `systems-depending-on' function"
  (is-true (position 'mutility/tests (systems-depending-on 'mutility) :key #'asdf:component-name :test #'string-equal)))

(test lisp-connections
  "Test the `lisp-connections' function"
  ;; FIX
  )

(test pathname-designator-p
  "Test the `pathname-designator-p' function"
  (is-true (pathname-designator-p "/home/blah.txt"))
  (is-true (pathname-designator-p #P"/home/blah.txt"))
  (is-false (pathname-designator-p nil))
  (is-false (pathname-designator-p '/home/blah.txt))
  (is-false (pathname-designator-p :foo)))

(test join-path-components
  "Test the `join-path-components' function"
  (is-true (string= "foo/bar/baz.qux" (join-path-components "foo" "/bar" "baz.qux")))
  (is-true (string= "/home/user/foo/bar/baz.qux" (join-path-components "/home/" "user" "foo" "/bar" "baz.qux")))
  (is-true (string= "temp/foo/bar/" (join-path-components "temp" "foo" "bar/"))))

(test open-url
  "Test the `open-url' function"
  ;; FIX
  )

(test generate-temporary-file-name
  "Test the `generate-temporary-file-name' function"
  ;; FIX
  )

(test locate-dominating-file
  "Test the `locate-dominating-file' function"
  ;; FIX
  )
