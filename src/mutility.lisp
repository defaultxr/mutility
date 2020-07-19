;;;; mutility.lisp

(in-package #:mutility)

;;; macros

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
             :for c = (elt list i)
             :for r = (list)
             :for nxt = nil ;; nxt means next value should be added to repeats even if it doesn't have !
             :append (progn
                       (when (symbolp c)
                         (when-let* ((sname (write-to-string c))
                                     (excl-pos (position #\! sname)))
                           (if (= 0 excl-pos)
                               (error "Failed to parse arguments for this invocation of `repeat-by-!': (a ~s ~s)" list add-list)
                               (let ((split (split-by-! sname)))
                                 (setf c (read-from-string (car split)))
                                 (setf r (mapcar 'eval (mapcar 'read-from-string (cdr split))))
                                 (when (char= #\! (elt sname (1- (length sname))))
                                   (setf nxt t))))))
                       (incf i)
                       (let ((continue t))
                         (loop :while (and continue (< i input-length))
                               :for chk = (elt list i)
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
                                               (if (and excl-pos (= 0 excl-pos))
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

(defmacro multiple-value-elt (value-form index)
  "Evaluates VALUE-FORM and returns the value at INDEX.

Example:

;; (multiple-value-elt (truncate 9/4) 1) ;=> 1/4

See also: `cl:multiple-value-list', `cl:multiple-value-bind'"
  `(elt (multiple-value-list ,value-form) ,index))

(defmacro dolist* ((item index list &optional result) &body body)
  "Like the standard `dolist' but includes INDEX as another variable representing the current index into LIST.

See also: `mapcar-with-index'"
  `(let ((,index 0))
     (dolist (,item ,list ,result)
       ,@body
       (incf ,index 1))))

;; (defmacro accumulating.nreverse (&body body)
;;   "Run BODY with the local function ACCUMULATE appending its values to a list, which is then returned.

;; See also: `accumulating', `uiop:while-collecting'."
;;   (let ((res-sym (gensym "RES"))
;;         (end-sym (gensym "END")))
;;     `(let* ((,res-sym (list)))
;;        (flet ((accumulate (value)
;;                 (push value ,res-sym)))
;;          ,@body
;;          (nreverse ,res-sym)))))

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

(defmacro define-obsolete-function-alias (old-function-name new-function-name) ;; from https://groups.google.com/forum/#!msg/comp.lang.lisp/uoHap8ZQKs8/simXrFNr_EYJ
  "Define an alias for an obsolete function. The alias will warn about the obsolete function when it is used."
  `(progn
     (defun ,old-function-name (&rest args) (apply #',new-function-name args))
     (define-compiler-macro ,old-function-name (&whole whole &rest args)
       (declare (ignore args))
       (warn "Function ~s is obsolete; please use ~s instead." ',old-function-name ',new-function-name)
       whole)))

;;; syntax sugar

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

(defun friendly-symbol (input &optional (package :keyword))
  "Return INPUT as a symbol, with all non-letter, non-number, and non-hyphen characters removed.

Example:

;; (friendly-symbol \"foo's bar, baz, and qux\") ;=> :FOOS-BAR-BAZ-AND-QUX

See also: `parse-boolean', `friendly-ratio-string', `friendly-duration-string'"
  (intern
   (let ((str (string-upcase
               (remove-if-not
                (lambda (letter)
                  (or (digit-char-p letter)
                      (alpha-char-p letter)
                      (char= #\- letter)))
                (substitute #\- #\_
                            (substitute #\- #\space (etypecase input
                                                      (string input)
                                                      (symbol (symbol-name input)))))))))
     (loop :for pos := (search "--" str)
           :if pos
             :do (setf str (concat (subseq str 0 pos) "-" (subseq str (+ 2 pos))))
           :else
             :do (loop-finish))
     str)
   package))

;;; strings

(defun concat (&rest objects)
  "Concatenates all OBJECTS together into a string (other than nils, which are skipped).

See also: `uiop:strcat'"
  (format nil "~{~A~}" (remove nil objects)))

(defun output (&rest items)
  "Concatenates and prints ITEMS, returning the last one.

See also: `concat'"
  (fresh-line)
  (format t "~{~A~}~%" (remove nil items))
  (finish-output)
  (car (last items)))

(defun split-string (string &key max-num (char-bag (list #\space #\tab #\newline)) include-empty)
  "Returns a list of substrings of 'string' divided by spaces, optionally splitting only to a list of a maximum size.

See also: `split-sequence', `str:split', `split-sequence:split-sequence'"
  (declare (type string string))
  (let ((char-bag (ensure-list char-bag)))
    (labels ((is-divider (char) (position char char-bag))
             (split-up (string num char-bag)
               (when (and string
                          (or include-empty
                              (not (equal string ""))))
                 (if (or (eql num 1)
                         (not (position-if #'is-divider string)))
                     (cons string nil)
                     (cons (subseq string 0 (position-if #'is-divider string))
                           (split-up (string-left-trim char-bag (subseq string (position-if #'is-divider string))) (when num (1- num)) char-bag))))))
      (split-up (if include-empty
                    string
                    (string-left-trim char-bag string))
                max-num char-bag))))

;; NOTE: shouldn't use this for long strings cuz it's not optimized
;; grabbed from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement.

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

(defun friendly-duration-string (seconds)
  "Format a number of seconds as a more human-readable string.

Example:

;; (friendly-duration-string 300) ;=> \"5:00\"
;; (friendly-duration-string 3600) ;=> \"1:00:00\"

See also: `friendly-ratio-string'"
  (let* ((min (truncate (/ seconds 60)))
         (hour (truncate (/ min 60)))
         (hourp (plusp hour))
         (min (mod min 60))
         (mins (write-to-string min))
         (sec (mod seconds 60))
         (secs (write-to-string sec)))
    (concat (when hourp
              (concat hour ":"))
            (if (and hourp (length= 1 mins))
                (concat "0" mins)
                mins)
            ":"
            (if (and (or hourp (plusp min))
                     (length= 1 secs))
                (concat "0" secs)
                secs))))

;;; packages

(defun my-intern (string &optional package)
  "Converts STRING into a symbol, uppercasing it in the process.

See also: `un-intern'"
  (if package
      (intern (string-upcase string) package)
      (intern (string-upcase string))))

(defun un-intern (symbol)
  "Converts a symbol into a string.

See also: `my-intern'"
  (string-downcase (write-to-string symbol)))

;;; math

;; use `alexandria:clamp' instead.
;; (defun clip (num &optional (bottom -1) (top 1))
;;   "Clips numbers within a range."
;;   (declare (type number num)
;;            (type number bottom)
;;            (type number top))
;;   (min top (max bottom num)))

(defun wrap (number &optional (bottom 0) (top 1))
  "Wraps a number between BOTTOM and TOP, similar to `cl:mod'.

Examples:

;; (wrap 2 0 1) ;; => 0
;; (wrap 5 0 10) ;; => 5
;; (wrap 15 0 10) ;; => 4

See also: `cl:mod', `alexandria:clamp', `within'"
  (declare (type number number)
           (type number bottom)
           (type number top))
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun floor-by (number &optional (by 1))
  "Round NUMBER down to the previous multiple of BY.

See also: `cl:floor', `round-by'"
  (/ (floor number by) (/ 1 by)))

(defun round-by (number &optional (by 1))
  "Round NUMBER to the nearest multiple of BY.

Examples:

;; (round-by 1 2) ;; => 0
;; (round-by 1.1 0.5) ;; => 1.0
;; (round-by 6 10) ;; => 10

See also: `cl:round', `round-by-direction' `floor-by'"
  (* (round (/ number by)) by))

(defun round-by-direction (number &optional (by 1))
  "Round NUMBER to the nearest multiple of BY. With positive BY, round up; with negative, round down.

Examples:

;; (round-by-direction 0.5 -1) ;; => 0
;; (round-by-direction 0.5 1) ;; => 1

See also: `round-by', `cl:round'"
  (if (= 0 (mod number by))
      number
      (let* ((positive (plusp by))
             (diff (cadr (multiple-value-list (funcall (if positive #'floor #'ceiling) number (abs by))))))
        (funcall (if positive #'+ #'-) number (funcall (if positive #'- #'+) (abs by) diff)))))

;;; lists and sequences

(defun length-upto (list &optional (max 10))
  "Get the length of LIST, not counting above MAX."
  (let ((res 0))
    (loop :for i :in list
          :repeat max
          :do (incf res))
    res))

(defun nth-wrap (n list)
  "Get the Nth item in LIST, wrapping the index if necessary.

Much like `nth', this function can only be used on lists. Use `elt-wrap' to index into any kind of sequence. However, keep in mind that `elt-wrap' may be slower when used on large lists.

See also: `elt-wrap'"
  (declare (type number n)
           (type cons list))
  (let ((next list))
    (loop :for i :from 0 :upto n
          :if (= i n)
            :return (car next)
          :else
            :do (if (cdr next)
                    (setf next (cdr next))
                    (setf next list)))))

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

(defun mapcar-with-index (function list &rest more-lists)
  "Like `mapcar', but provides the index of the current element as an additional final element to FUNCTION."
  (let ((index -1))
    (apply #'mapcar (lambda (&rest args)
                      (incf index)
                      (apply function (append args (list index))))
           (append (list list) more-lists))))

(defun flatten-1 (list)
  "Like `alexandria:flatten', but only flattens one layer."
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

See also: `exponential-random-range', `gauss'"
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

See also: `random-range', `gauss'"
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

;; save a hash table (from https://www.youtube.com/watch?v=njfyWgqZmkI )
(defun save-hash-table (ht filename &key overwrite)
  "Save a hash table to a file. See `restore-hash-table' to load the saved table."
  (let (list)
    (flet ((save-slot (key value)
             (push (cons key value) list)))
      (maphash #'save-slot ht))
    (with-open-file (f filename :direction :output :if-exists (if overwrite overwrite :error))
      (prin1 list f))))

(defun restore-hash-table (filename &rest make-hash-table-args)
  "Restore a hash table from a file saved with the `save-hash-table' function."
  (let ((ht (apply #'make-hash-table make-hash-table-args)))
    (with-open-file (f filename)
      (dolist (cell (read f))
        (let ((key (car cell))
              (value (cdr cell)))
          (setf (gethash key ht) value))))
    ht))

;;; unsorted

(defun current-seconds ()
  "Get the number of seconds that Lisp has been running for."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun open-url (url)
  "Open a URL via the OS's default application."
  (uiop:launch-program (list #+linux "xdg-open"
                             #+darwin "open"
                             #+windows "start"
                             url)))

(defun generate-temporary-file-name (&key name (directory "/tmp") extension attempt)
  "Generate a string representing a full path to a new temporary file. The file name defaults to a timestamp. Will automatically create DIRECTORY if it doesn't exist. Will also attempt to generate a new name if a file with that name already exists. ATTEMPT is the attempt number if the filename already exists.

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
                      (string (list :absolute
                                    (string-left-trim
                                     (list (uiop:directory-separator-for-host))
                                     directory)))
                      (pathname directory)
                      (list directory)))
         (type (typecase extension
                 (symbol
                  (string-downcase (string extension)))
                 (string extension)))
         (res (namestring
               (make-pathname :directory directory
                              :name (concat name (when attempt
                                                   (concat "-" attempt)))
                              :type type))))
    (if (uiop:file-exists-p res)
        (generate-temporary-file-name :name name :directory directory :extension type :attempt (if attempt (1+ attempt) 1))
        res)))

;; conditionally load swank-extensions if swank is available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (featurep :swank)
    (load (asdf:system-relative-pathname :mutility "src/extensions/swank-extensions.lisp"))))
