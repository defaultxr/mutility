;;;; t/queue.lisp - tests for mutility queue implementation.

(in-package #:mutility/tests)

(in-suite mutility-tests)

(test queue
  "Test `queue' and associated functions"
  (let ((q (make-queue 4)))
    (signals queue-empty
      (queue-dequeue q)
      "queue-dequeue doesn't signal queue-empty for empty queues")
    (signals queue-empty
      (queue-peek q)
      "queue-peek doesn't signal queue-empty for empty queues")
    (queue-enqueue q 0)
    (queue-enqueue q 1)
    (queue-enqueue q 2)
    (is (eql 0 (queue-peek q))
        "queue-peek doesn't return the oldest item")
    (is (eql 0 (queue-dequeue q))
        "queue-dequeue doesn't return the oldest item")
    (is (eql 1 (queue-dequeue q))
        "queue-dequeue doesn't return the second-oldest item")
    (queue-enqueue q 3)
    (is (eql 2 (queue-dequeue q))
        "queue-dequeue doesn't return the third-oldest item")
    (queue-enqueue q 4)
    (queue-enqueue q 5)
    (queue-enqueue q 6)
    (signals queue-full
      (queue-enqueue q 7)
      "queue-enqueue doesn't signal queue-full for full queues")
    (is (equal (list 3 4 5 6)
               (queue-contents q))
        "queue-contents returns incorrect results")
    (dotimes (n 4)
      (queue-dequeue q)
      (queue-enqueue q n))
    (is (equal (list 0 1 2 3)
               (queue-contents q))
        "queue-contents returns incorrect results after queue-index wraps")
    (signals queue-index-out-of-range
      (queue-elt q 4)
      "queue-elt doesn't signal queue-index-out-of-range for indexes too high")
    (signals queue-index-out-of-range
      (queue-elt q -5)
      "queue-elt doesn't signal queue-index-out-of-range for indexes too low")
    (is (eql 0 (queue-elt q 0))
        "queue-elt doesn't return the correct oldest value")
    (is (eql 1 (queue-elt q 1))
        "queue-elt doesn't return the correct second-oldest value")
    (is (eql 3 (queue-elt q -1))
        "queue-elt doesn't return the correct newest value")
    (is (eql 2 (queue-elt q -2))
        "queue-elt doesn't return the correct second-newest value")
    (is (equal (queue-contents q)
               (let (res)
                 (do-queue (item q (nreverse res))
                   (push item res))))
        "do-queue doesn't iterate against a queue correctly")))

;; (test queue-sequence
;;   "Test `queue''s sequence functionality"
;;   (if (not (find-package "SEQUENCE"))
;;       (skip "Sequence functionality is not supported on this implementation")
;;       (let ((rb (make-queue 5)))
;;         (is (= 5 (length rb))
;;             "queue's length result is incorrect"))))
