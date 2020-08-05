(in-package #:mutility)

(defun mapcar* (function list &rest more-lists)
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
           (append (list list) more-lists))))

;; FIX:
;; (defun mapplist (function list &rest more-lists)
;;   (mapcar ))

;; (defmacro mapplist (function &rest lists)

;;   (with-gensyms (key value)
;;     `(loop ,@(loop :for list :in lists
;;                    :append (list :for (,key ,value) :on ,list :by #'cddr
;;                                  ))))
;;   (mapcar ))

(defmacro dolist* ((item index list &optional result) &body body)
  "Like the standard `cl:dolist' but includes the current index into LIST as another variable, INDEX.

Example:

;; (dolist* (item index (list 'this 'that 'the-other-thing))
;;   (format t \"Item #~s is ~s!~%\" index item))
;; ;=> nil
;; output:
;; Item #0 is THIS!
;; Item #1 is THAT!
;; Item #2 is THE-OTHER-THING!

See also: `cl:dolist', `mapcar*', `cl:dotimes'"
  `(let ((,index 0))
     (dolist (,item ,list ,result)
       ,@body
       (incf ,index 1))))

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
  "Run BODY with the local function ACCUMULATE appending its values to a list, which is then returned. This macro avoids having to reverse the list at the end like with the traditional `push'/`nreverse' idiom.

See also: `uiop:while-collecting'."
  (let ((res-sym (gensym "RES"))
        (end-sym (gensym "END")))
    `(let* ((,res-sym (list nil))
            (,end-sym ,res-sym))
       (flet ((,(ensure-symbol 'accumulate) (value)
                (setf (cdr ,end-sym) (cons value nil)
                      ,end-sym (cdr ,end-sym))))
         ,@body
         (cdr ,res-sym)))))

(export (list 'mapcar*
              'dolist*
              'while
              'do-while
              'until
              'accumulating))
