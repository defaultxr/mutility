;;;; mutility.lisp

(in-package #:mutility)

;;; macros & sugar

(defun split-by-! (string)
  "Split STRING up by exclamation points."
  (remove-if #'emptyp (split-sequence string #\!)))

(defun repeat-by (object repeats &optional add-list)
  "Returns a list of object repeated REPEATS times. If REPEATS is a list of multiple numbers, recursively repeat the generated lists.

When ADD-LIST is true, prepend 'list to each generated list.

Example:

;; (repeat-by 3 3)
;; => (3 3 3)
;;
;; (repeat-by 3 '(3 2))
;; => ((3 3 3) (3 3 3))

See also: `repeat-by-!', `a'"
  (let* ((repeats (ensure-list repeats))
         (list (append (when add-list
                         (list 'list))
                       (make-list (car repeats) :initial-element object))))
    (if (cdr repeats)
        (repeat-by list (cdr repeats) add-list)
        list)))

(defun prepend-list-to-sublists (list)
  "Prepend the symbol 'list to LIST and all of its sublists."
  (if (listp list)
      (cons 'list (mapcar (lambda (x)
                            (if (listp x)
                                (prepend-list-to-sublists x)
                                x))
                          list))
      list))

(defun repeat-by-! (list &optional add-list)
  "Given LIST, repeat items marked with ! by the number after the !.

When ADD-LIST is true, prepend 'list to each generated list. This is useful if you're using this function in a macro, such as the `a' macro, which this function does all the heavy lifting for.

Examples:

;; (repeat-by-! '(1!2))
;; => (1 1)
;;
;; (repeat-by-! '(1!2!3))
;; => ((1 1) (1 1) (1 1))
;;
;; (repeat-by-! '(1 (* 2 3)!2))
;; => (1 (* 2 3) (* 2 3))

See also: `repeat-by', `a'"
  (flet ((get-repeats (item)
           (etypecase item
             (symbol
              (mapcar 'eval (mapcar 'read-from-string (split-by-! (write-to-string item)))))
             (integer
              (list item))
             (list
              (list (eval item))))))
    (let ((i 0)
          (input-length (length list)))
      (append
       (when add-list
         (list 'list))
       (loop :until (>= i input-length)
             :for c := (elt list i)
             :for r := (list)
             :for nxt := nil ;; nxt means next value should be added to repeats even if it doesn't have !
             :append (progn
                       (when (symbolp c)
                         (when-let* ((sname (write-to-string c))
                                     (excl-pos (position #\! sname)))
                           (if (zerop excl-pos)
                               (error "Failed to parse arguments for this invocation of `repeat-by-!': (a ~s ~s)" list add-list)
                               (let ((split (split-by-! sname)))
                                 (setf c (read-from-string (car split)))
                                 (setf r (mapcar 'eval (mapcar 'read-from-string (cdr split))))
                                 (when (char= #\! (elt sname (1- (length sname))))
                                   (setf nxt t))))))
                       (incf i)
                       (let ((continue t))
                         (loop :while (and continue (< i input-length))
                               :for chk := (elt list i)
                               :do (progn
                                     (if nxt
                                         (progn
                                           (setf r (append r (get-repeats chk))
                                                 nxt nil)
                                           (when (and (symbolp chk)
                                                      (let ((sname (write-to-string chk)))
                                                        (char= #\! (elt sname (1- (length sname))))))
                                             (setf nxt t)))
                                         (if (symbolp chk)
                                             (let* ((sname (write-to-string chk))
                                                    (excl-pos (position #\! sname)))
                                               (if (and excl-pos (zerop excl-pos))
                                                   (progn
                                                     (setf r (append r (mapcar 'eval (mapcar 'read-from-string (split-by-! sname)))))
                                                     (when (char= #\! (elt sname (1- (length sname))))
                                                       (setf nxt t)))
                                                   (setf continue nil)))
                                             (setf continue nil)))
                                     (when continue
                                       (incf i)))))
                       (let ((res (repeat-by c (or r 1) add-list)))
                         (if add-list
                             (cdr res)
                             res))))))))

(defun expand-ranges (list)
  "Expand ranges denoted by a..b in list.

Example:

;; (expand-ranges '(0..5 -2..2))
;; => (0 1 2 3 4 5 -2 -1 0 1 2)"
  (loop :for i :in list
        :append (typecase i
                  (symbol
                   (let* ((sname (symbol-name i))
                          (pos (search ".." sname)))
                     (if pos
                         (let ((first (parse-integer (subseq sname 0 pos)))
                               (last (parse-integer (subseq sname (+ 2 pos)))))
                           (loop :for n :from first :to last :collect n))
                         (list i))))
                  (t (list i)))))

(defmacro a (&rest args)
  "Quickly and conveniently generate lists. Use ! to denote repetition of the previous element, or .. to denote a range.

Inspired by similar functionality in SuperCollider.

Examples:

;; (a 3!3)
;; => (3 3 3)
;;
;; (a -5 (random 3)!5 9 10)
;; => (-5 0 2 2 1 2 9 10)
;;
;; (a 2..9)
;; => (2 3 4 5 6 7 8 9)

See also: `repeat-by-!', `expand-ranges'"
  (expand-ranges (repeat-by-! args t)))

(defmacro fn (&body body)
  "Syntax sugar for making `lambda's. BODY is the function body. Underscores in the body can be used to represent the argument to the function."
  (let ((args (list)))
    (labels ((parse (list)
               (mapcar (lambda (i)
                         (typecase i
                           (list (parse i))
                           (symbol (if (string= "_" i)
                                       (progn
                                         (pushnew i args)
                                         i)
                                       i))
                           (t i)))
                       list)))
      (let ((body (parse body)))
        `(lambda (,@args) ,@body)))))

(defmacro with-access (slots instance &body body)
  "Like `with-accessors' and `with-slots' combined; any slots provided as symbols are assumed to refer to both the variable name and the accessor. If no such accessor exists, just grab the slot as per `with-slots'.

Example:

If FOO is a function and BAR is not:

;; (with-access (foo bar) blah
;;   (format t \"~s ~s~%\" foo bar))

...is the same as:

;; (with-accessors ((foo foo)) blah
;;   (with-slots (bar) blah
;;     (format t \"~s ~s~%\" foo bar)))

See also: `cl:with-accessors', `cl:with-slots'"
  (multiple-value-bind (accessors slots)
      (uiop:while-collecting (a s)
        (dolist (slot slots)
          (cond
            ((listp slot)
             (a slot))
            ((fboundp slot)
             (a (list slot slot)))
            (t
             (s slot)))))
    (let ((slots-form (if slots
                          `((with-slots (,@slots) ,instance
                              ,@body))
                          body)))
      (if accessors
          `(with-accessors (,@accessors) ,instance
             ,@slots-form)
          (car slots-form)))))

(defmacro define-obsolete-function-alias (old-function-name new-function-name) ;; from https://groups.google.com/forum/#!msg/comp.lang.lisp/uoHap8ZQKs8/simXrFNr_EYJ
  "Define an alias for an obsolete function. The alias will warn about the obsolete function when it is used."
  `(progn
     (defun ,old-function-name (&rest args) (apply #',new-function-name args))
     (define-compiler-macro ,old-function-name (&whole whole &rest args)
       (declare (ignore args))
       (warn "Function ~s is obsolete; please use ~s instead." ',old-function-name ',new-function-name)
       whole)))

(defmacro dprint (&rest args)
  "Easy macro to get debug output for a list of variables, ARGS. For each argument in ARGS, print the argument itself, then print what it evaluates to. Returns the last value.

Example:

;; (dprint (random 10) (+ 2 2))

...prints something like the following:
(RANDOM 10): 6; (+ 2 2): 4;
...and returns the value 4."
  (with-gensyms (results)
    `(let ((,results (list ,@args)))
       (fresh-line)
       (loop :for name :in ',args
             :for val :in ,results
             :do (format t "~s: ~s; " name val)
             :finally (progn
                        (terpri)
                        (return val))))))

(defgeneric keys (object)
  (:documentation "Get the keys of OBJECT, whether it be a plist, event, etc."))

(defmethod keys ((object null))
  nil)

(defmethod keys ((object cons))
  (labels ((accum (list)
             (cons (car list)
                   (when (cddr list)
                     (accum (cddr list))))))
    (accum object)))

(defmethod keys ((object hash-table))
  (hash-table-keys object))

;;; symbols

(defun my-intern (string &optional package)
  "Converts STRING into a symbol, uppercasing it in the process.

See also: `reintern', `un-intern'"
  (if package
      (intern (string-upcase string) package)
      (intern (string-upcase string))))

(defun reintern (symbol &optional (package *package*))
  "Reintern a symbol, changing its package to PACKAGE.

See also: `my-intern', `un-intern'"
  (intern (symbol-name symbol) package))

(defun un-intern (symbol)
  "Converts a symbol into a string.

See also: `reintern', `my-intern'"
  (string-downcase (write-to-string symbol)))

(defun friendly-string (input)
  "Return INPUT as a string with all non-letter, non-number, and non-hyphen characters removed.

Example:

;; (friendly-symbol \"foo's bar, baz, and qux\") ;=> :FOOS-BAR-BAZ-AND-QUX

See also: `friendly-symbol', `parse-boolean', `friendly-ratio-string', `friendly-duration-string'"
  (let ((str (remove-if-not
              (lambda (letter)
                (or (digit-char-p letter)
                    (alpha-char-p letter)
                    (char= #\- letter)))
              (substitute #\- #\_
                          (substitute #\- #\space (etypecase input
                                                    (string input)
                                                    (symbol (symbol-name input))))))))
    (loop :for pos := (search "--" str)
          :if pos
            :do (setf str (concat (subseq str 0 pos) "-" (subseq str (+ 2 pos))))
          :else
            :do (loop-finish))
    str))

(defun friendly-symbol (input &optional (package :keyword))
  "Return INPUT as a symbol with all non-letter, non-number, and non-hyphen characters removed.

Example:

;; (friendly-symbol \"foo's bar, baz, and qux\") ;=> :FOOS-BAR-BAZ-AND-QUX

See also: `friendly-string', `parse-boolean', `friendly-ratio-string', `friendly-duration-string'"
  (intern (string-upcase (friendly-string input)) package))

;;; strings

(defun concat (&rest objects)
  "Concatenates all OBJECTS together into a string (other than nils, which are skipped).

See also: `cl:concatenate', `uiop:strcat'"
  (format nil "~{~A~}" (remove nil objects)))

(defun output (&rest items)
  "Concatenates and prints ITEMS, returning the last one.

See also: `concat'"
  (fresh-line)
  (format t "~{~A~}~%" (remove nil items))
  (finish-output)
  (car (last items)))

(defun split-string (string &key max-num (char-bag (list #\space #\tab #\newline)) include-empty)
  "Split STRING into a list of substrings by partitioning by the characters in CHAR-BAG, optionally to a list of maximum size MAX-NUM. If INCLUDE-EMPTY is true, include empty strings in the resulting list (and length count); otherwise exclude them.

Example:

;; (split-string \"this that the other thing\")
;; ;=> (\"this\" \"that\" \"the\" \"other\" \"thing\")

;; (split-string \"  foo  bar baz  qux  \" :max-num 2)
;; ;=> (\"foo\" \"bar baz  qux  \")

See also: `split-sequence', `str:split', `split-sequence:split-sequence'"
  (declare (type string string))
  (let ((char-bag (ensure-list char-bag)))
    (labels ((divider-p (char)
               (position char char-bag))
             (split-up (string num char-bag)
               (when (and string
                          (or include-empty
                              (not (emptyp string))))
                 (if (or (eql num 1)
                         (not (position-if #'divider-p string)))
                     (cons string nil)
                     (cons (subseq string 0 (position-if #'divider-p string))
                           (split-up (string-left-trim char-bag (subseq string (position-if #'divider-p string))) (when num (1- num)) char-bag))))))
      (split-up (if include-empty
                    string
                    (string-left-trim char-bag string))
                max-num char-bag))))

;; NOTE: shouldn't use this for long strings cuz it's not optimized
;; grabbed from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Get a new string in which all the occurences of the part is replaced with replacement.

See also: `cl-ppcre:regex-replace-all'"
  (with-output-to-string (out)
    (loop :with part-length := (length part)
          :for old-pos := 0 :then (+ pos part-length)
          :for pos := (search part string
                              :start2 old-pos
                              :test test)
          :do (write-string string out
                            :start old-pos
                            :end (or pos (length string)))
          :when pos :do (write-string replacement out)
            :while pos)))

(defun parse-boolean (string &optional default)
  "Parse STRING as a boolean, returning either t or nil, or DEFAULT if it is not a known boolean string."
  (cond
    ((null string) default)
    ((member string (list "t" "1" "true" "y" "yes" "e" "enable" "enabled" "on") :test #'string-equal) t)
    ((member string (list "nil" "0" "f" "false" "n" "no" "d" "disable" "disabled" "off") :test #'string-equal) nil)
    (t default)))

(defun friendly-ratio-string (ratio &optional (separator " ")) ;; FIX: negative numbers are weird
  "Format a ratio as a more human-readable string.

Example:

;; (friendly-ratio-string 5/4) ;=> \"1 1/4\"
;; (friendly-ratio-string 9/7) ;=> \"1 2/7\"

See also: `friendly-duration-string'"
  (etypecase ratio
    (ratio
     (if (> (abs ratio) 1)
         (let* ((flr (floor ratio))
                (rem (- ratio flr)))
           (concat flr (unless (zerop rem)
                         (concat separator rem))))
         (let ((denom (denominator ratio)))
           (concat (numerator ratio) (unless (= 1 denom)
                                       (concat "/" denom))))))
    (number
     (write-to-string ratio))))

;; FIX: make this calculate days, weeks, etc as well?
(defun friendly-duration-string (seconds &key include-ms)
  "Format a number of seconds as a more human-readable string. For now, hours are the biggest unit considered.

Example:

;; (friendly-duration-string 300) ;=> \"5:00\"
;; (friendly-duration-string 3600) ;=> \"1:00:00\"

See also: `friendly-ratio-string'"
  (let* ((min (truncate (/ seconds 60)))
         (hour (truncate (/ min 60)))
         (hourp (plusp hour))
         (min (mod min 60))
         (sec (mod seconds 60)))
    (multiple-value-bind (sec frac) (truncate sec)
      (concat (when hourp
                (concat hour ":"))
              (format nil (if hourp "~2,'0d" "~d") min)
              (format nil ":~2,'0d" sec)
              (when include-ms
                (format nil ".~3,'0d" (truncate (* 1000 frac))))))))

;; FIX: ensure this works with Lisp's built-in indentation functionality?
(defun pretty-print-tree (tree &optional (indent 0))
  "Pretty print TREE, indenting the elements of each sublist."
  (dolist (e tree)
    (if (listp e)
        (pretty-print-tree e (1+ indent))
        (progn
          (dotimes (n indent)
            (princ "  "))
          (format t "- ~s~%" e)))))

;;; math

(defun wrap (number &optional (bottom 0) (top 1))
  "Wraps a number between BOTTOM and TOP, similar to `cl:mod'.

Examples:

;; (wrap 2 0 1) ;; => 0
;; (wrap 5 0 10) ;; => 5
;; (wrap 15 0 10) ;; => 4

See also: `fold', `cl:mod', `alexandria:clamp'"
  (declare (type number number)
           (type number bottom)
           (type number top))
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun fold (number &optional (bottom 0) (top 1))
  "Fold numbers outside BOTTOM and TOP back into the range.

Examples:

;; (fold -1 0 1) ;=> 1
;; (fold 5 0 10) ;=> 5
;; (fold 8 0 7) ;=> 6

See also: `wrap', `cl:mod', `alexandria:clamp'"
  (declare (type number number)
           (type number bottom)
           (type number top))
  (if (>= top number bottom)
      number
      (let* ((range (- top bottom))
             (modded (mod (- number bottom) (* 2 range))))
        (if (>= modded range)
            (+ top (- range modded))
            (+ bottom modded)))))

(defun floor-by (number &optional (by 1))
  "Round NUMBER down to the previous multiple of BY.

See also: `cl:floor', `ceiling-by', `round-by'"
  (/ (floor number by) (/ 1 by)))

(defun ceiling-by (number &optional (by 1))
  "Round NUMBER up to the next multiple of BY.

See also: `cl:ceiling', `floor-by', `round-by'"
  (/ (ceiling number by) (/ 1 by)))

(defun round-by (number &optional (by 1))
  "Round NUMBER to the nearest multiple of BY.

Examples:

;; (round-by 1 2) ;; => 0
;; (round-by 1.1 0.5) ;; => 1.0
;; (round-by 6 10) ;; => 10

See also: `cl:round', `floor-by', `ceiling-by'"
  (* (round (/ number by)) by))

(uiop:with-deprecation (:warning)
  (defun round-by-direction (number &optional (by 1))
    "Deprecated; use either `floor-by', `ceiling-by', or `round-by' instead."
    (if (zerop (mod number by))
        number
        (let* ((positive (plusp by))
               (diff (cadr (multiple-value-list (funcall (if positive #'floor #'ceiling) number (abs by))))))
          (funcall (if positive #'+ #'-) number (funcall (if positive #'- #'+) (abs by) diff))))))

;;; lists and sequences

(defun list-length-upto (list &optional (max 10))
  "Get the length of LIST, not counting above MAX.

Example:

;; (length-upto (make-list 200) 20) ;=> 20

See also: `alexandria:length='"
  (let ((res 0))
    (loop :for i :in list
          :repeat max
          :do (incf res))
    res))

(uiop:with-deprecation (:style-warning)
  (defun length-upto (list &optional (max 10))
    "Deprecated and renamed to `list-length-upto'."
    (apply #'list-length-upto list (list max))))

(defun list-length>= (list n)
  "True if LIST is at least N in length. Probably more efficient than doing something like (>= (length list) n).

Example:

;; (list-length>= (make-list 300) 10) ;=> T

See also: `list-length>', `list-length-upto', `alexandria:length='"
  (let ((current 0))
    (dolist (item list nil)
      (incf current)
      (when (>= current n)
        (return-from list-length>= t)))))

(defun list-length> (list n)
  "True if LIST is more than N in length.

See also: `list-length>=', `alexandria:length='"
  (list-length>= list (1+ n)))

(defun nth-wrap (n list)
  "Get the Nth item in LIST, wrapping the index if necessary.

Much like `nth', this function can only be used on lists. Use `elt-wrap' to index into any kind of sequence. However, keep in mind that `elt-wrap' may be slower when used on large lists.

See also: `elt-wrap'"
  (declare (type number n)
           (type cons list))
  (nth (mod n (list-length list)) list))

(defun elt-wrap (sequence n)
  "Get the Nth item in SEQUENCE, wrapping the index if necessary.

Much like `elt', this function can be used on any sequence. However, because this function calls `length' to determine the wrapped index, it may be slow when used on large lists. Consider using `nth-wrap' in those cases instead.

See also: `nth-wrap'"
  (declare (type number n)
           (type sequence sequence))
  (elt sequence (mod n (length sequence))))

(defun find-any (items list &key test)
  "Returns the first item from ITEMS that is found in LIST, or nil if none."
  (dolist (item items)
    (when (position item list :test test)
      (return-from find-any item))))

(defun most (function list &key (key #'identity)) ;; from https://stackoverflow.com/questions/30273802/how-would-i-get-the-min-max-of-a-list-using-a-key
  "Get the most FUNCTION item in LIST by comparing the KEY of each item with FUNCTION. Unlike `reduce', this function returns the whole item from LIST, even when KEY is provided.

Example:

;; get the item in the list with the smallest car:
;; (most '< '((2 :bar) (3 :baz) (1 :foo)) :key 'car) ;=> (1 :FOO)

See also: `cl:reduce', `cl:find-if'"
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall function e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

(defun flatten-1 (list)
  "Like `alexandria:flatten', but only flattens one layer.

Example:

;; (flatten-1 '(1 (2 (3 4) 5) 6))
;; ;=> (1 2 (3 4) 5 6)

See also: `alexandria:flatten'"
  (apply #'append (mapcar #'ensure-list list)))

(defun subseq* (sequence start &optional end)
  "Like subseq, but allows start and end to be negative."
  (let ((length (length sequence)))
    (subseq sequence (if (< start 0)
                         (+ length start)
                         start)
            (if (and (numberp end) (< end 0))
                (+ length end)
                end))))

(defun repeat (item num)
  "Get a list containing NUM ITEMs. If ITEM is a function, return a list of NUM of the result of that function."
  (declare (integer num))
  (assert (typep num '(integer 0)) (num) "NUM must be a positive integer; got ~s instead." num)
  (the list
       (when (plusp num)
         (cons (if (eql 'function (type-of item))
                   (funcall item)
                   item)
               (repeat item (- num 1))))))

(defun split-sequence (sequence delimiter)
  "Split SEQUENCE by DELIMITER."
  (if-let ((pos (position delimiter sequence)))
    (cons (subseq sequence 0 pos) (split-sequence (subseq sequence (1+ pos)) delimiter))
    (cons sequence nil)))

(defun left-trim (bag list &key (test #'eql))
  "Trim anything from BAG from the start of LIST.

See also: `cl:string-left-trim'"
  (member-if-not (lambda (x) (position x (ensure-list bag) :test test)) list))

(defmacro affixnew (place thing)
  "Affix THING to the end of PLACE if it's not already a member.

See also: `alexandria:appendf', `cl:pushnew'."
  (once-only (thing)
    `(unless (position ,thing ,place)
       (appendf ,place (list ,thing)))))

(defun insert-if (function list item)
  "Destructively insert ITEM into LIST at the position where FUNCTION is true. If the function doesn't return true, the item is inserted at the end of the list. Similar to `nreverse', the result is returned ;; FIX

Example:

;; (insert-if #'plusp (list -2 -1 1 2) 0)
;; ;; => (-2 -1 0 1 2)

See also: `insert-sorted'"
  (if list
      (progn
        (loop :for i :on list
              :if (funcall function (car i))
                :do (setf (cdr i) (cons (car i) (cdr i))
                          (car i) item)
                    (loop-finish)
              :when (null (cdr i))
                :do (setf (cdr i) (cons item nil))
                    (loop-finish))
        list)
      (list item)))

(defun insert-sorted (list number)
  "Destructively insert NUMBER into LIST in order.

Example:

;; (insert-sorted (list 1 2 3 4) 2.5)
;; ;; => (1 2 2.5 3 4)

See also: `insert-if'"
  (insert-if (fn (>= _ number)) list number))

;;; randomness

(defun random-coin (&optional (probability 0.5))
  "Randomly return true with a probability of PROBABILITY/1."
  (<= (random 1.0) probability))

(defun random-range (low &optional high)
  "Return a random number between LOW and HIGH, inclusive. If HIGH is not provided, act the same as (random LOW).

See also: `exponential-random-range', `random-gauss'"
  (if high
      (let ((rval (- high low)))
        (+ low
           (random (if (integerp rval)
                       (1+ rval)
                       rval))))
      (random low)))

(defun random-range.new (low &optional high) ;; version 2, with support for ratios - FIX
  "Return a random number between LOW and HIGH, inclusive. If HIGH is not provided, act the same as (random LOW)."
  (flet ((rnd (number)
           (if (typep number 'ratio)
               (/ (random (1+ (numerator number))) (denominator number))
               (random number))))
    (if high
        (let ((rval (- high low)))
          (+ low
             (rnd (if (integerp rval)
                      (1+ rval)
                      rval))))
        (rnd low))))

(defun exponential-random-range (low high) ;; adapted from supercollider/include/plugin_interface/SC_RGen.h
  "Generate a random number between LOW and HIGH, with exponential distribution.

See also: `random-range', `random-gauss'"
  (* low
     (exp (* (log (/ high
                     low))
             (random 1d0)))))

(defun random-gauss (mean standard-deviation)
  "Generate a random number from a normal (Gaussian) distribution.

See also: `random-range', `exponential-random-range'"
  (let* ((first-random (random 1.0))
         (sqrt-result (if (equal first-random 0.0)
                          (sqrt most-positive-single-float)
                          (sqrt (* -2 (log first-random))))))
    (+ (* sqrt-result
          (sin (random (* 2 pi)))
          standard-deviation)
       mean)))

;;; hash tables

(defun save-hash-table (hash filename &key (if-exists :error))
  "Save a hash table to a file. See `restore-hash-table' to load the saved table.

Example:

;; (save-hash-table *my-hash* \"/home/user/blah.hash\" :if-exists :rename)

See also: `restore-hash-table'"
  (with-open-file (stream filename :direction :output :if-exists if-exists)
    (princ "(" stream)
    (maphash (lambda (key value)
               (print (cons key value) stream))
             hash)
    (fresh-line stream)
    (princ ")" stream)))

(defun restore-hash-table (filename &rest make-hash-table-args)
  "Restore a hash table from a file saved with the `save-hash-table' function.

Example:

;; (restore-hash-table \"/home/user/blah.hash\")
;; ;=> #<HASH-TABLE ...>

See also: `save-hash-table'"
  (let ((hash (apply #'make-hash-table make-hash-table-args)))
    (with-open-file (stream filename)
      (dolist (cell (read stream))
        (setf (gethash (car cell) hash) (cdr cell))))
    hash))

;;; introspection

(defun current-seconds ()
  "Get the number of seconds that Lisp has been running for."
  (/ (get-internal-real-time) internal-time-units-per-second))

;; swiped from https://stackoverflow.com/questions/15465138/find-functions-arity-in-common-lisp
(defun function-arglist (function)
  "Get the signature of FUNCTION."
  #+allegro (excl:arglist function)
  #+clisp (sys::arglist function)
  #+(or cmu scl)
  (let ((f (coerce function 'function)))
    (typecase f
      (standard-generic-function (pcl:generic-function-lambda-list f))
      (eval:interpreted-function (eval:interpreted-function-arglist f))
      (function (values (read-from-string (kernel:%function-arglist f))))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase function (symbol (fdefinition function)) (t function)))
  #+gcl (let ((function (etypecase function
                          (symbol function)
                          (function (si:compiled-function-name function)))))
          (get function 'si:debug))
  #+lispworks (lw:function-lambda-list function)
  #+lucid (lcl:arglist function)
  #+sbcl (sb-introspect:function-lambda-list function)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list 'arglist function)))

(defun lisp-connections ()
  "Get a list of the current connections to this Lisp image."
  (when-let ((package (cond ((find-package 'slynk) 'slynk)
                            ((find-package 'swank) 'swank))))
    (symbol-value (find-symbol "*CONNECTIONS*" package))))

;;; file utilities

(defun open-url (url)
  "Open a URL via the OS's default application."
  (uiop:launch-program (list #+linux "xdg-open"
                             #+darwin "open"
                             #+windows "start"
                             url)))

(defun generate-temporary-file-name (&key name (directory (uiop:temporary-directory)) extension)
  "Generate a string representing a full path to a new temporary file. The file name defaults to a timestamp. Will automatically create DIRECTORY if it doesn't exist. Will also attempt to generate a new name if a file with that name already exists.

Example:

;; (generate-temporary-file-name :name \"foo\" :directory \"/tmp/lisp/\" :extension \"wav\")
;; => \"/tmp/lisp/foo.wav\"

;; (generate-temporary-file-name :directory \"/tmp/lisp/\" :extension :flac)
;; => \"/tmp/lisp/2020-04-20-06-09-00.flac\""
  (uiop:ensure-pathname directory :ensure-directories-exist t)
  (let* ((name (or name
                   (local-time:format-timestring nil (local-time:now) ;; FIX: make the local-time dependency optional?
                                                 :format (list
                                                          (list :year 4) #\-
                                                          (list :month 2) #\-
                                                          (list :day 2) #\-
                                                          (list :hour 2) #\-
                                                          (list :min 2) #\-
                                                          (list :sec 2)))))
         (directory (etypecase directory
                      ((or string pathname list) directory)))
         (type (typecase extension
                 (null nil)
                 (symbol
                  (string-downcase (string extension)))
                 (string extension)))
         (attempt 0))
    (flet ((gen-filename (attempt)
             (make-pathname :directory (pathname-directory directory)
                            :name (format nil "~a~@[-~3,'0d~]" name (unless (zerop attempt) attempt))
                            :type type)))
      (loop :while (uiop:file-exists-p (gen-filename attempt))
            :do (incf attempt))
      (namestring (gen-filename attempt)))))

(defun locate-dominating-file (directory name)
  "Starting at DIRECTORY, look for a file named NAME in the current directory and successive parents, returning the first one found or nil if none.

Similar to the Emacs function \"locate-dominating-file\"."
  (labels ((try-next (directory)
             (let ((check (uiop:merge-pathnames* name directory)))
               (if (file-exists-p check)
                   check
                   (let ((next (parent-directory directory)))
                     (if (uiop:pathname-equal directory next)
                         nil
                         (try-next next)))))))
    (when-let ((res (try-next (ensure-directory-trailing-slash (full-path directory)))))
      (if (stringp directory)
          (namestring res)
          res))))

;; conditionally load emacs-extensions if swank or slynk are available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (or (featurep :swank)
            (featurep :slynk))
    (load (asdf:system-relative-pathname :mutility "src/extensions/emacs-extensions.lisp"))))
