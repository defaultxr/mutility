;;;; mutility.lisp

(in-package #:mutility)

;;; macros

;; FIX:
;; (defmacro fn (&body body)
;;   ""
;;   (labels ((parse (list)
;;              (mapcar (lambda (i)
;;                        (typecase i
;;                          (list (parse i))
;;                          (atom (if (string= "_" i)
;;                                    'arg
;;                                    i))))
;;                      list)))
;;     (parse body)))

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
       (flet ((accumulate (value)
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

;;; string stuff

(defun concat (&rest objects)
  "Concatenates all OBJECTS together into a string (other than nils, which are skipped).

See also: `uiop:strcat'"
  (format nil "~{~A~}" (remove-if #'null objects)))

;;; see instead: `alexandria:symbolicate'
;; (defun concats (&rest symbols)
;;   "Concatenates symbols. The package of the first symbol will be used as the package of the resulting symbol."
;;   (intern (apply 'concatenate 'string (mapcar 'symbol-name symbols)) (symbol-package (car symbols))))

(defun output (&rest items)
  "Concatenates and prints ITEMS, returning the last one.

See also: `concat'"
  (fresh-line)
  (format t "~{~A~}~%" (remove-if #'null items))
  (finish-output)
  (car (last items)))

(defun split (string &key max-num (char-bag (list #\space #\tab #\newline)))
  "Returns a list of substrings of 'string' divided by spaces, optionally splitting only to a list of a maximum size.

See also: `split-sequence:split-sequence'"
  (declare (type string string)
           (type cons char-bag))
  (labels ((is-divider (char) (position char char-bag))
           (split-up (string num char-bag)
             (when (and string
                        (not (equal string "")))
               (if (or (eq num 1)
                       (not (position-if #'is-divider string)))
                   (cons (string-right-trim char-bag string) nil)
                   (cons (subseq string 0 (position-if #'is-divider string))
                         (split-up (string-left-trim char-bag (subseq string (position-if #'is-divider string))) (when num (1- num)) char-bag))))))
    (split-up (string-left-trim char-bag string) max-num char-bag)))

;; NOTE: shouldn't use this for long strings cuz it's not optimized
(defun replace-all (string part replacement &key (test #'char=)) ;; stolen from http://cl-cookbook.sourceforge.net/strings.html
  "Returns a new string in which all the occurences of the part is replaced with replacement.

See also: `cl-ppcre:regex-replace-all'"
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

;;; package stuff

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

;;; math stuff

;; use `alexandria:clamp' instead.
;; (defun clip (num &optional (bottom -1) (top 1))
;;   "Clips numbers within a range."
;;   (declare (type number num)
;;            (type number bottom)
;;            (type number top))
;;   (min top (max bottom num)))

(defun wrap (number &optional (bottom 0) (top 1))
  "Wraps the input number to be within the top and bottom bounds, inclusive."
  (declare (type number number)
           (type number bottom)
           (type number top))
  (+ (mod (- number bottom) (- (1+ top) bottom)) bottom))

;;; list/sequence stuff

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

(defun has-any (items list &key test)
  "Returns the first item from ITEMS that is found in LIST, or nil if none."
  (dolist (item items)
    (when (position item list :test test)
      (return-from has-any item))))

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

;;; random stuff

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
  (+ (* (sqrt (* -2 (log (random 1.0))))
        (sin (random (* 2 pi)))
        standard-deviation)
     mean))

;;; hash stuff

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

;;; unsorted stuff

(defun current-seconds ()
  "Get the number of seconds that Lisp has been running for."
  (/ (get-internal-real-time) internal-time-units-per-second))
