;;;; loopy.lisp - mutility looping constructs

(in-package #:mutility)

(defun mapcar* (function list &rest more-lists) ; FIX: this can be simplified/improved probably; also needs tests.
  "Like `mapcar', but provides the index of the current element as an additional final element to FUNCTION.

Example:

;; (mapcar*
;;  (lambda (item index)
;;    (format nil \"Item ~s is ~s!\" index item))
;;  (list 'this 'that 'the-other-thing))
;; ;=> (\"Item 0 is THIS!\" \"Item 1 is THAT!\" \"Item 2 is THE-OTHER-THING!\")

See also: `cl:mapcar', `dolist*'"
  (let ((index -1))
    (apply #'mapcar (lambda (&rest args)
                      (incf index)
                      (apply function (append args (list index))))
           list more-lists)))

(defun mapplist (function &rest lists)
  "Like `mapcar', but for property lists; FUNCTION is applied to successive pairs of elements at the head of each listin LISTS. The return value is the appended list of each result of calling FUNCTION. Mapplist returns as soon as the shortest list ends.

Example:

;; (mapplist (lambda (k v) (list k (1+ v)))
;;           '(:foo 1 :bar 2))
;; ;=> (:FOO 2 :BAR 3)
;;
;; (mapplist (lambda (w x y z) (list w (1+ x) y (+ 2 z)))
;;           '(:foo 1 :bar 2)
;;           '(:baz 3 :qux 4))
;; ;=> (:FOO 2 :BAZ 5 :BAR 3 :QUX 6)

See also: `cl:mapcar', `mapcar*', `doplist'"
  (uiop:while-collecting (results)
    (tagbody
     mapplist-loop
       (dolist (i (ensure-list (apply function (mapcan (lambda (x) (subseq x 0 2)) lists))))
         (results i))
       (setf lists (mapcar (lambda (list)
                             (or (cddr list)
                                 (go mapplist-exit)))
                           lists))
       (go mapplist-loop)
     mapplist-exit)))

(import '(trivial-do:dolist* trivial-do:doalist trivial-do:dohash trivial-do:doseq trivial-do:doseq*))

(defmacro dorange ((var from to &optional by result) &body body)
  "Execute BODY multiple times, with VAR bound to a number starting at FROM and increasing by BY after each iteration until VAR passes or is equal to TO. Finally, RESULT is returned.

Example:

;; (dorange (v 0 5 2 t)
;;   (print v)) ;; prints 0, 2, and 4, then returns T

See also: `cl:dotimes', `cl:loop', `cl:dolist'"
  (assert (not (eql 0 by)) (by) "BY argument cannot be zero.")
  (with-gensyms (tosym signsym startsym)
    `(let* ((,var ,from)
            (,tosym ,to)
            (,signsym (signum (- ,var ,tosym))))
       (block nil
         (tagbody
            ,startsym
            ,@body
            (incf ,var ,(or by `(- ,signsym)))
            (unless (or (= ,signsym (signum (- ,tosym ,var)))
                        (= ,from ,tosym))
              (go ,startsym)))
         ,result))))

(defmacro while (test &body body)
  "If TEST is true, run BODY, then loop back to the beginning. Returns the last item in TEST or BODY before the end of the loop.

Example:

;; (let ((foo 0))
;;   (while (< foo 3)
;;     (incf foo)))
;; => 3

See also: `do-while', `until', `cl:loop'"
  (with-gensyms (testsym ressym varsym)
    `(let (,ressym)
       (tagbody
          ,testsym
          (when-let ((,varsym ,test))
            (setf ,ressym (or (progn ,@body)
                              ,varsym))
            (go ,testsym)))
       ,ressym)))

;; FIX: these should return the last item, even if it's just the result of the test (i.e. it should be possible to leave BODY empty)

(defmacro do-while (test &body body)
  "Run BODY, then loop back to the beginning if TEST is true. Like `while', but always runs BODY at least once.

See also: `while', `cl:loop'"
  (with-gensyms (testsym)
    `(tagbody
        ,testsym
        ,@body
        (when ,test
          (go ,testsym)))))

(defmacro until (test &body body)
  "If TEST is false, run BODY, then loop back to the beginning.

See also: `while', `cl:loop'"
  `(while (not ,test) ,@body))

#+nil
(defmacro accumulating.nreverse (&body body)
  "Run BODY with the local function ACCUMULATE appending its values to a list, which is then returned.

See also: `accumulating', `uiop:while-collecting'."
  (let ((res-sym (gensym "RES"))
        (end-sym (gensym "END")))
    `(let* ((,res-sym (list)))
       (flet ((accumulate (value)
                (push value ,res-sym)))
         ,@body
         (nreverse ,res-sym)))))

(defmacro accumulating (&body body)
  "Run BODY with a local functions ACCUMULATE, which appends its input to a list, and RESET-ACCUMULATION, which clears the accumulated list. The accumulated list is returned by the accumulating block when it exits.

This macro avoids having to reverse the list at the end like with the traditional `push'/`nreverse' idiom.

Example:

;; (accumulating
;;   (accumulate 4)
;;   (reset-accumulation)
;;   (accumulate 5)
;;   (dotimes (n 3)
;;     (accumulate 6)))
;; ;=> (5 6 6 6)

See also: `uiop:while-collecting'."
  (let ((res-sym (gensym "RES"))
        (end-sym (gensym "END")))
    `(let* ((,res-sym (list nil))
            (,end-sym ,res-sym))
       (flet ((,(ensure-symbol 'accumulate) (value)
                (setf (cdr ,end-sym) (cons value nil)
                      ,end-sym (cdr ,end-sym)))
              (,(ensure-symbol 'reset-accumulation) ()
                (setf ,res-sym (list nil)
                      ,end-sym ,res-sym)))
         (declare (ignorable #',(ensure-symbol 'reset-accumulation)))
         ,@body
         (cdr ,res-sym)))))

(export (list 'mapcar*
              'mapplist
              'trivial-do:doalist
              'trivial-do:dohash
              'trivial-do:dolist*
              'trivial-do:doseq
              'trivial-do:doseq*
              'dorange
              'while
              'do-while
              'until
              'accumulating))
