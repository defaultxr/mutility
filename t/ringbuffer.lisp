;;;; t/ringbuffer.lisp - tests for mutility ringbuffer implementation.

(in-package #:mutility/tests)

(in-suite mutility-tests)

(test ringbuffer-1
  "First set of `ringbuffer' tests"
  (let ((rb (make-ringbuffer 3 'initial-element)))
    (is (= 3 (ringbuffer-size rb))
        "ringbuffer-size doesn't return the specified size")
    (is-false (let (res)
                (do-ringbuffer (var rb res)
                  (push var res)))
              "do-ringbuffer iterates elements even when the ringbuffer is empty")
    (is (eql 'initial-element (ringbuffer-initial-element rb))
        "ringbuffer-initial-element doesn't return the ringbuffer's specified initial element")
    (is (= 0 (ringbuffer-length rb))
        "ringbuffer-length returns the wrong result for fresh ringbuffers")
    (is (= 0 (ringbuffer-index rb))
        "ringbuffer-index returns the wrong result for fresh ringbuffers")
    (is (eql 'initial-element (ringbuffer-elt rb -1))
        "ringbuffer-elt doesn't return the initial element when called on a fresh ringbuffer")
    (ringbuffer-push rb 'foo)
    (is (equal (list 'foo)
               (let (res)
                 (do-ringbuffer (var rb res)
                   (push var res))))
        "do-ringbuffer doesn't iterate correctly against a ringbuffer with one element")
    (is (= 1 (ringbuffer-length rb))
        "ringbuffer-length returns the wrong result for ringbuffers with one element")
    (is (= 1 (ringbuffer-index rb))
        "ringbuffer-index returns the wrong result for ringbuffers with one element")
    (is (eql 'foo (ringbuffer-elt rb -1))
        "ringbuffer-elt doesn't return the most recent element when called on a ringbuffer with one element")
    (ringbuffer-push rb 'bar)
    (is (equal (list 'bar 'foo)
               (let (res)
                 (do-ringbuffer (var rb res)
                   (push var res))))
        "do-ringbuffer doesn't iterate correctly against a ringbuffer with two elements")
    (is (= 2 (ringbuffer-length rb))
        "ringbuffer-length returns the wrong result for ringbuffers with two elements")
    (is (= 2 (ringbuffer-index rb))
        "ringbuffer-index returns the wrong result for ringbuffers with two elements")
    (is (eql 'bar (ringbuffer-elt rb -1))
        "ringbuffer-elt doesn't return the most recent element when called on a ringbuffer with two elements")
    (is (eql 'bar (ringbuffer-pop rb))
        "ringbuffer-pop doesn't return the most recent element")
    (is (eql 'foo (ringbuffer-elt rb -1))
        "ringbuffer-elt doesn't return the most recent element after ringbuffer-pop is used")
    (is (= 1 (ringbuffer-index rb))
        "ringbuffer-index doesn't return the right index after ringbuffer-pop is used")
    (is (= 1 (ringbuffer-length rb))
        "ringbuffer-length doesn't return the right length after ringbuffer-pop is used")
    (ringbuffer-push rb 'bar)
    (ringbuffer-push rb 'baz)
    (is (equal (list 'baz 'bar 'foo)
               (let (res)
                 (do-ringbuffer (var rb res)
                   (push var res))))
        "do-ringbuffer doesn't iterate correctly against a ringbuffer with three elements")
    (is (= 0 (ringbuffer-index rb))
        "ringbuffer-index should wrap back to 0 when ringbuffer-size net elements are pushed")
    (is (= (ringbuffer-size rb) (ringbuffer-length rb))
        "ringbuffer-length should max out at ringbuffer-size")
    (is (eql 'baz (ringbuffer-elt rb -1)))
    (is (eql 'baz (ringbuffer-pop rb)))
    (is (= 2 (ringbuffer-index rb)))
    (is (eql 'bar (ringbuffer-elt rb -1)))
    (ringbuffer-push rb 'baz)
    (ringbuffer-push rb 'qux)
    (is (equal (list 'qux 'baz 'bar)
               (let (res)
                 (do-ringbuffer (var rb res)
                   (push var res))))
        "do-ringbuffer doesn't iterate correctly against a wrapped ringbuffer with three elements")
    (is (eql 'qux (ringbuffer-elt rb -1)))
    (is (eql 'baz (ringbuffer-elt rb -2)))
    (is (eql 'bar (ringbuffer-elt rb -3)))
    (is (eql 'qux (ringbuffer-elt rb -4)))
    (is (eql 'qux (ringbuffer-pop rb)))
    (is (eql 'bar (ringbuffer-get rb)))
    (is (eql 'baz (ringbuffer-pop rb)))
    (is (eql 'initial-element (ringbuffer-pop rb)))
    (is (eql 'initial-element (ringbuffer-get rb)))))

(test ringbuffer-2
  "Second set of `ringbuffer' tests"
  (let ((rb (make-ringbuffer 5 'initial-element)))
    (ringbuffer-push rb 0)
    (ringbuffer-push rb 1)
    (ringbuffer-push rb 2)
    (is (eql 0 (ringbuffer-get rb))
        "ringbuffer-get returns incorrect results for the first element pushed")
    (is (eql 1 (ringbuffer-get rb))
        "ringbuffer-get returns incorrect results for the second element pushed")
    (is (eql 2 (ringbuffer-get rb))
        "ringbuffer-get returns incorrect results for the third element pushed")))

(test ringbuffer-3
  "Third set of `ringbuffer' tests"
  (let ((rb (make-ringbuffer 5 'initial-element)))
    (dotimes (n 3)
      (ringbuffer-push rb n))
    (is (equal (list 2 1 0 'initial-element 'initial-element)
               (ringbuffer-newest rb 5))
        "ringbuffer-newest returns incorrect results")
    (is (equal (list 'initial-element 'initial-element 0 1 2)
               (ringbuffer-oldest rb 5))
        "ringbuffer-oldest returns incorrect results")
    (dotimes (n 3)
      (ringbuffer-push rb (+ 10 n)))
    (is (equal (list 12 11 10 2 1)
               (ringbuffer-newest rb 5))
        "ringbuffer-newest returns incorrect results after values fall off the end")
    (is (equal (list 1 2 10 11 12)
               (ringbuffer-oldest rb 5))
        "ringbuffer-oldest returns incorrect results after values fall off the end")))

(test ringbuffer-4
  "Fourth set of `ringbuffer' tests"
  (let ((rb (make-ringbuffer 3 'initial-element)))
    (dotimes (i 3)
      (ringbuffer-push rb i))
    (is (eql 0 (ringbuffer-elt rb 0))
        "ringbuffer-elt returns incorrect results for the oldest item pre-wrapping")
    (is (eql 1 (ringbuffer-elt rb 1))
        "ringbuffer-elt returns incorrect results for the second oldest item pre-wrapping")
    (is (eql 2 (ringbuffer-elt rb -1))
        "ringbuffer-elt returns incorrect results for the newest item pre-wrapping")
    (is (eql 1 (ringbuffer-elt rb -2))
        "ringbuffer-elt returns incorrect results for the second newest item pre-wrapping")
    (dotimes (i 2)
      (ringbuffer-push rb (+ 5 i)))))
