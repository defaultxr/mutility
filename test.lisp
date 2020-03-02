(in-package #:mutility)

(defpackage #:mutility/tests
  (:use #:cl
        #:mutility
        #:fiveam))

(in-package #:mutility/tests)

(def-suite mutility-tests
    :description "mutility test suite.")

(in-suite mutility-tests)

(test accumulating
  "Test accumulating"
  (is (equal (list 1 2 3)
             (mutility::accumulating
               (mutility::accumulate 1)
               (mutility::accumulate 2)
               (mutility::accumulate 3))))
  (is (equal (list 0 2 4 6 8)
             (mutility::accumulating
               (dotimes (n 5)
                 (mutility::accumulate (* 2 n)))))))

(test split
  "Test split"
  (is (equal (list "" "")
             (split "d" :char-bag (list #\d) :include-empty t)))
  (is (null (split "d" :char-bag (list #\d))))
  (is (equal (list "this" "that" "the" "other" "thing")
             (split "this that
the other thing"))))
