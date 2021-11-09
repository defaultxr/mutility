(in-package #:mutility/tests)

(in-suite mutility-tests)

(test mapcar*
  "Test the `mapcar*' macro"
  ;; FIX
  )

(test dorange
  "Test the `dorange' macro"
  (is (equal (list 5 4 3 2 1 0)
             (let (res)
               (dorange (v 0 5 1 res)
                 (push v res))))
      "dorange has incorrect behavior when counting from 0 to 5 by 1")
  (is (equal (list 0 1 2 3 4 5)
             (let (res)
               (dorange (v 5 0 -1 res)
                 (push v res))))
      "dorange has incorrect behavior when counting from 5 to 0 by -1")
  (is (equal (list 0 1 2 3 4 5)
             (let (res)
               (dorange (v 5 0 nil res)
                 (push v res))))
      "dorange doesn't guess BY correctly when none is provided")
  (is (equal (list 2 5/3 4/3 1 2/3 1/3 0)
             (let (res)
               (dorange (v 0 2 1/3 res)
                 (push v res))))
      "dorange has incorrect behavior when counting from 5 to 0 by -1")
  (is-true (dorange (v 0 2 1 nil)
             (return t))
           "dorange does not create an implicit block")
  (is-true (let ((n 0))
             (dorange (v 0 0 1 n)
               (incf n 1)
               (when (> n 1)
                 (return nil))))
           "dorange loops too many times when FROM and TO are equal"))

(test while
  "Test the `while' macro"
  ;; FIX
  )

(test do-while
  "Test the `do-while' macro"
  ;; FIX
  )

(test until
  "Test the `until' macro"
  ;; FIX
  )

(test accumulating
  "Test the `accumulating' macro"
  ;; FIX
  )
