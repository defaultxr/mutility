;;;; mutility.lisp - the main mutility functionality.

(in-package #:mutility)

;;; macros & sugar

(defun split-by-! (string)
  "Split STRING up by exclamation points."
  (remove-if #'emptyp (sequence-split string #\!)))

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
             :for nxt := nil ; nxt means next value should be added to repeats even if it doesn't have !
             :append (progn
                       (when (symbolp c)
                         (when-let* ((sname (write-to-string c))
                                     (excl-pos (position #\! sname)))
                           (if (zerop excl-pos)
                               (error "Failed to parse arguments for this invocation of `repeat-by-!': (~S ~S ~S)" 'a list add-list)
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
  (mapcan (lambda (i)
            (typecase i
              (symbol
               (let* ((sname (symbol-name i))
                      (pos (search ".." sname)))
                 (if pos
                     (let ((first (parse-integer (subseq sname 0 pos)))
                           (last (parse-integer (subseq sname (+ 2 pos)))))
                       (loop :with dir := (signum (- last first))
                             :with n := first
                             :collect n
                             :until (= n last)
                             :do (incf n dir)))
                     (list i))))
              (t (list i))))
          list))

(defmacro a (&rest args)
  "Quickly and conveniently generate lists. Use ! to denote repetition of the previous element, or .. to denote a range.

Inspired by similar functionality in SuperCollider.

Examples:

;; (a 3!3)
;; ;=> (3 3 3)

;; (a -5 (random 3)!5 9 10)
;; ;=> (-5 0 2 2 1 2 9 10)

;; (a 2..9)
;; ;=> (2 3 4 5 6 7 8 9)

See also: `fn', `repeat-by-!', `expand-ranges'"
  (expand-ranges (repeat-by-! args t)))

;; see also f-underscore; https://gitlab.common-lisp.net/bpm/f-underscore/-/blob/master/f-underscore.lisp
(defmacro fn (&body body)
  "Syntax sugar for `lambda'. BODY is the function body. Symbols consisting of an underscore and a number are treated as the lambda's argument at that index. For example, _1 is the second argument of the lambda. A single underscore is treated the same as _0.

Examples:

;; (funcall (fn (list _1 _0)) :foo :bar) ;=> (:BAR :FOO)

;; (funcall (fn (/ 3 _1)) :foo 2) ;=> 3/2

See also: `cut', `a'"
  (let ((args (list))
        (max -1))
    (labels ((fn-sym-p (thing)
               (when (and (symbolp thing)
                          (not (string= "" thing))
                          (char= #\_ (char (string thing) 0)))
                 (let* ((str (string thing))
                        (maybe-num (subseq str 1)))
                   (when (emptyp maybe-num)
                     (setf max 0)
                     (let ((res (intern "_0")))
                       (pushnew res args)
                       (return-from fn-sym-p res)))
                   (when (digit-char-p (char maybe-num 0))
                     (let ((pos-not (position-if-not #'digit-char-p maybe-num)))
                       (if pos-not
                           (return-from fn-sym-p nil) ; FIX: support ".." notation for "this arg and rest"?
                           (progn
                             (setf max (max max (parse-integer maybe-num)))
                             (pushnew thing args)
                             (return-from fn-sym-p thing))))))))
             (parse (list)
               (mapcar (lambda (i)
                         (typecase i
                           (list (parse i))
                           (symbol (or (fn-sym-p i)
                                       i))
                           (t i)))
                       list)))
      (let ((body (parse body))
            (unused (list)))
        `(lambda (,@(loop :for num :from 0 :upto max
                          :for var := (intern (format nil "_~D" num))
                          :collect var
                          :unless (member var args :test #'string=)
                            :do (pushnew var unused)))
           ,@(when unused
               (list (list 'declare (list* 'ignore unused))))
           ,@body)))))

(defmacro cut (func &rest args)
  "The cut macro; notation for specializing parameters without currying, as described in SRFI 26.

https://srfi.schemers.org/srfi-26/srfi-26.html

Examples:

;; (cut '/ 1 <>) ;=> (lambda (x) (/ 1 x))
;; (cut <> 1 2) ;=> (lambda (func) (funcall func 1 2))
;; (cut '+ <> <>) ;=> (lambda (x y) (+ x y))

See also: `fn'"
  (flet ((<>-p (i)
           (and (symbolp i)
                (string= '<> i))))
    (let* ((gensyms (when (<>-p func)
                      (list (gensym))))
           (arg-symbols (if (<>-p func)
                            (copy-list gensyms)
                            (list func))))
      (dolist (arg args)
        (push (if (<>-p arg)
                  (let ((sym (gensym)))
                    (push sym gensyms)
                    sym)
                  arg)
              arg-symbols))
      `(lambda (,@(nreverse gensyms)) (funcall ,@(nreverse arg-symbols))))))

(uiop:with-deprecation (:warning)
  (defmacro with-access (slots instance &body body)
    "Deprecated; recommended to use metabang-bind's \"bind\" macro instead.

Like `with-accessors' and `with-slots' combined; any slots provided as symbols are assumed to refer to both the variable name and the accessor. If no such accessor exists, just grab the slot as per `with-slots'.

Example:

If FOO is a function and BAR is not:

;; (with-access (foo bar) blah
;;   (format t \"~S ~S~%\" foo bar))

...is the same as:

;; (with-accessors ((foo foo)) blah
;;   (with-slots (bar) blah
;;     (format t \"~S ~S~%\" foo bar)))

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
            (car slots-form))))))

(defclass funcallable-wrapper ()
  ((funcall-function :initarg funcall-function :accessor funcallable-wrapper-funcall-function :type function :documentation "The function to call when this object is `funcall'ed."))
  (:documentation "Simple wrapper class to apply funcallable object functionality to funcallable objects defined with `defclass+'.")
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((object funcallable-wrapper) &key)
  (closer-mop:set-funcallable-instance-function
   object
   (lambda (&rest args)
     (apply (slot-value object 'funcall-function) object args))))

;; FIX: any reader/writer/accessor method defined with defclass+ should have its documentation set to the slot's definition if it doesn't already have documentation.
;; FIX: can we make it so that the instance is automatically available as the "this" variable in the funcallable-wrapper function?
(defmacro defclass+ (name direct-superclasses &body body)
  "`cl:defclass' convenience wrapper. Features a much more succinct syntax, additionally defining common functions for the class such as the predicate function.

Adds the following features to `cl:defclass':

- Docstring can be specified as the first element of BODY, similar to `cl:defun'.
- Automatically defines a NAME-p (predicate) function.
- Adds a :function option which can be used to specify what to call when the object is `cl:funcall'ed. The metaclass is also automatically set to `closer-mop:funcallable-standard-class'.

Example:

;; (defclass+ foo ()
;;   \"Example class defined with defclass+\"
;;   (a-slot :initarg :a-slot :initform 3)
;;   (:function 'print))
;;
;; (funcall (make-instance 'foo)) ; since print is the :function, this is the same as (print (make-instance 'foo))

See also: `cl:defclass'"
  (flet ((option-p (slot)
           "True if SLOT is an \"option\" (i.e. its first element is a keyword)."
           (keywordp (car slot)))
         (option-value (option)
           "Get the value of the option slot named OPTION."
           (car (assoc-value body option))))
    (let* ((name-string (string-downcase name))
           (test-symbol (upcase-intern (concat name "-P") *package*))
           (docstring (cond ((stringp (car body))
                             (prog1
                                 (car body)
                               (setf body (cdr body))))
                            ((option-value :documentation)
                             (option-value :documentation))))
           (direct-slots (remove-if #'option-p body))
           (options (remove-if-not #'option-p body))
           (function (option-value :function))
           (funcallable (or function
                            (find-if (fn (typep _ 'closer-mop:funcallable-standard-class))
                                     (mapcar 'find-class direct-superclasses)))))
      (dolist (option (list :function :documentation))
        (deletef options option :key #'car))
      (when docstring
        (push `(:documentation ,docstring) options))
      (when funcallable
        (push `(:metaclass closer-mop:funcallable-standard-class) options)
        (when function
          (appendf direct-slots `((funcall-function :initform ,function))))
        (appendf direct-superclasses (list 'funcallable-wrapper)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (prog1
             (defclass ,name ,direct-superclasses
               (,@direct-slots)
               ,@options)
           (defun ,test-symbol (object)
             ,(concat "Test whether OBJECT is a" (when (vowel-char-p (char name-string 0)) "n") " `" name-string "'.")
             (typep object ',name)))))))

(define-condition no-dictionary-entry (error)
  ((entry :initarg :entry :reader no-dictionary-entry-entry :documentation "The name of the entry being looked up.")
   (dictionary-name :initarg :dictionary-name :reader no-dictionary-entry-dictionary-name :documentation "The name of the dictionary.")
   (dictionary :initarg :dictionary :reader no-dictionary-entry-dictionary :documentation "The dictionary object itself."))
  (:report
   (lambda (condition stream)
     (format stream "~@<No entry ~S found in the ~S dictionary.~@:>"
             (no-dictionary-entry-entry condition)
             (no-dictionary-entry-dictionary-name condition))))
  (:documentation "Condition for when a dictionary entry is not found."))

(setf (documentation 'no-dictionary-entry-entry 'function) "The name of the entry being looked up."
      (documentation 'no-dictionary-entry-dictionary-name 'function) "The name of the dictionary."
      (documentation 'no-dictionary-entry-dictionary 'function) "The dictionary object itself.")

;; FIX: check usecases in bdef, cyxnxdx, kyndlr, etc and ensure that this will work for them too.
;; FIX: auto-detect NAME-TYPE by default by looking at the :type of the NAME class slot.
;; FIX: should we use a mixin class for this instead? https://lisp.substack.com/p/mixins-in-common-lisp
;; FIX: should we provide an :after method for (setf name) ?
(defmacro define-dictionary (name &key (name-type 'symbol) (include-errorp t) (errorp-default t) (define-class-functions :if-class-exists) find-function-name)
  "Define a \"dictionary\" named NAME that maps symbols to objects. Defines the *NAME-dictionary* hash table and several functions for access to said table and the associated objects.

Functions defined:

- NAME-p - Test whether an object is an instance of NAME (if NAME is a class).
- find-NAME - Find the object in the dictionary with the specified name.
- (setf find-NAME) - Set the object in the dictionary with the specified name.
- all-NAMEs - Get a list of all of the objects in the dictionary.
- all-NAME-names - Get a list of all symbols defined in the dictionary.
- NAME-names - Get a list of all names in the dictionary that point to the specified object, optionally including aliases.
- If NAME is the name of a class and DEFINE-CLASS-FUNCTIONS is true, also define methods specializing on symbols for each of that class's accessors, which look up the object pointed to by that symbol and return the value of that method being called on said object.

Options:

- NAME-TYPE - The type that a NAME dictionary name (key) can be; typically symbol, string, or string-designator.
- INCLUDE-ERRORP - Whether to include the errorp keyword argument for find-NAME.
- ERRORP-DEFAULT - The default value for find-NAME's errorp argument.
- DEFINE-CLASS-FUNCTIONS - If t, define functions and methods for the class named NAME and error if no such class exists. If :if-class-exists, define methods if the class exists but don't error otherwise. If nil, don't define any methods even if the class exists.
- FIND-FUNCTION-NAME - The name that should be used to define the find-NAME function. Defaults to find-NAME.

Example:

;; (define-dictionary foo)
;;
;; (setf (find-foo 'bar) (list 1 2 3))
;;
;; (find-foo 'bar) ;=> (1 2 3)

See also: `make-hash-table', `find-class', `do-symbols'"
  (let* ((name-string (string-downcase name))
         (dict-symbol (upcase-intern (concat "*" name "-dictionary*") *package*))
         (test-symbol (upcase-intern (concat name '-p) *package*))
         (find-symbol (or find-function-name (upcase-intern (concat 'find- name) *package*)))
         (class (and define-class-functions (find-class name nil)))
         (class-slots (when class
                        (closer-mop:class-direct-slots class)))
         (class-method-names (when class
                               (mapcar (fn (closer-mop:generic-function-name (closer-mop:method-generic-function _)))
                                       (remove-if-not (fn (typep _ 'closer-mop:standard-reader-method))
                                                      (closer-mop:specializer-direct-methods class)))))
         (has-name (find 'name class-slots :key #'closer-mop:slot-definition-name :test #'string=)))
    (when (and (eql t define-class-functions)
               (not class))
      (error "DEFINE-CLASS-FUNCTIONS is t, but no class named ~S could be found." name))
    (when (and class define-class-functions (not has-name))
      (warn "Found class ~S but it doesn't appear to have a NAME slot." class))
    `(progn
       (defvar ,dict-symbol (make-hash-table :test ',(if (string= 'symbol name-type) 'eql 'equal))
         ,(concatenate 'string "The " name-string " dictionary hash table.

See also: `find-" name-string "'"))
       (defun ,test-symbol (object)
         ,(concat "Test whether OBJECT is a" (when (vowel-char-p (char name-string 0)) "n") " " name-string ".

See also: `find-" name-string "'")
         (typep object ',name))
       (defun ,find-symbol (name &key ,@(when include-errorp (list (list 'errorp errorp-default))) (dictionary ,dict-symbol))
         ,(concat "Get the object named NAME in the " name-string " dictionary. If ERRORP is true, signals an error when NAME isn't found in the dictionary. Returns t or nil as a second value showing whether the symbol was found.

See also: `" name-string "-p', `all-" name-string "s', `all-" name-string "-names'")
         (check-type name (and (or ,name ,name-type) (not null)))
         (if (,test-symbol name)
             name
             (if-let ((res (gethash name dictionary)))
               (if (typep res ',name-type) ; values that are of type NAME-TYPE are considered aliases that point to the dictionary object of the specified name.
                   (,find-symbol res ,@(when include-errorp (list :errorp 'errorp)) :dictionary dictionary)
                   res)
               ,(if include-errorp
                    `(when errorp
                       (error 'no-dictionary-entry :entry name :dictionary-name ',name-string :dictionary ,dict-symbol))
                    (when errorp-default
                      `(error 'no-dictionary-entry :entry name :dictionary-name ',name-string :dictionary ,dict-symbol))))))
       (defun (setf ,find-symbol) (value name &key errorp (dictionary ,dict-symbol))
         (declare (ignore errorp))
         (setf (gethash name dictionary) value))
       (defun ,(upcase-intern (concat 'all- name 's) *package*) (&key package (dictionary ,dict-symbol))
         ,(concat "Get a list of all defined " name-string " objects in DICTIONARY. With PACKAGE, get only " name-string "s whose name is a symbol in that package.

See also: `all-" name-string "-names', `" name-string "-names', `find-" name-string "'")
         (let ((objects (remove-if-not #',test-symbol (hash-table-values dictionary))))
           (if package
               (let ((package (etypecase package
                                (package package)
                                (symbol (find-package package)))))
                 (remove-if-not (fn (eql (symbol-package _) package)) objects))
               objects)))
       (defun ,(upcase-intern (concat 'all- name '-names) *package*) (&key (include-aliases t) package (dictionary ,dict-symbol))
         ,(concat "Get a list of the names of all defined " name-string " objects.

See also: `all-" name-string "s', `" name-string "-names'")
         (let ((names (if include-aliases
                          (keys dictionary)
                          (let (res)
                            (maphash (lambda (k v)
                                       (unless (typep v ',name-type)
                                         (push k res)))
                                     dictionary)
                            res))))
           (if package
               (let ((package (etypecase package
                                (package package)
                                (symbol (find-package package)))))
                 (remove-if-not (fn (and (symbolp _)
                                         (eql (symbol-package _) package)))
                                names))
               names)))
       ,@(when has-name
           `((defun ,(upcase-intern (concat name '-names) *package*) (name &key (include-aliases t) (dictionary ,dict-symbol))
               ,(concat "Get a list of all the names in DICTIONARY that point to NAME.

See also: `all-" name-string "-names', `all-" name-string "s'")
               (when-let* ((object (,find-symbol name))
                           (object-name (slot-value object ',(closer-mop:slot-definition-name has-name))))
                 (loop :for key :being :the hash-keys :of dictionary
                         :using (hash-value value)
                       :if (or (eq value object)
                               (and include-aliases
                                    (or (equalp value object-name)
                                        (when (typep value ',name-type)
                                          (eq object (,find-symbol value))))))
                         :collect key)))))
       ,@(when (and class class-method-names)
           (mapcar (lambda (method)
                     `(progn
                        (defmethod ,method ((symbol symbol))
                          (,method (,find-symbol symbol)))
                        (defmethod ,method ((null null))
                          nil)))
                   class-method-names)))))

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
             :do (format t "~S: ~S; " name val)
             :finally (terpri)
                      (return val)))))

(defgeneric keys (object)
  (:documentation "Get the keys of OBJECT, whether it be a plist, hash table, etc."))

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

(defun upcase-intern (string &optional (package *package*))
  "Uppercase and convert STRING into a symbol.

See also: `alexandria:ensure-symbol', `string-downcase'"
  (intern (string-upcase string) package))

(uiop:with-deprecation (:warning)
  (defun my-intern (string &optional (package *package*))
    "Deprecated alias for `upcase-intern'."
    (upcase-intern string package))

  (defun reintern (symbol &optional (package *package*))
    "Deprecated function; recommend using `alexandria:ensure-symbol' instead."
    (intern (symbol-name symbol) package))

  (defun un-intern (symbol)
    "Deprecated alias for `string-downcase'."
    (string-downcase symbol)))

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

(defun concat (&rest objects) ; FIX: conflicts with `serapeum:concat', which differs in that it doesn't concatenate symbols.
  "Concatenate all non-nil OBJECTS together into a string.

See also: `cl:concatenate', `uiop:strcat'"
  (format nil "~{~@[~A~]~}" objects))

(defun output (&rest objects)
  "Concatenate (as per `concat') and print OBJECTS, returning the last one.

See also: `concat'"
  (format t "~&~{~@[~A~]~}~%" objects)
  (finish-output)
  (lastcar objects))

(defun vowel-char-p (char)
  "True if CHAR is a vowel character (i.e. a, e, i, o, or u). Y and w are not tested.

See also: `cl:alpha-char-p', `cl:digit-char-p', `cl:graphic-char-p', `cl:standard-char-p'"
  (member char (list #\a #\e #\i #\o #\u) :test #'char-equal))

(defun string-designator-p (object)
  "True if OBJECT is a string-designator, i.e. a string or symbol.

See also: `alexandria:string-designator'"
  (typep object 'string-designator))

(uiop:with-deprecation (:warning)
  (defun split-string (&rest rest)
    "Deprecated alias for `string-split'."
    (apply #'string-split rest)))

(defun string-split (string &key (char-bag (list #\space #\tab #\newline)) count include-empty max-num)
  "Split STRING into a list of substrings by partitioning by the characters in CHAR-BAG, optionally to a list of maximum size COUNT. If INCLUDE-EMPTY is true, include empty strings in the resulting list (and length count); otherwise exclude them.

Example:

;; (split-string \"this that the other thing\")
;; ;=> (\"this\" \"that\" \"the\" \"other\" \"thing\")

;; (split-string \"  foo  bar baz  qux  \" :count 2)
;; ;=> (\"foo\" \"bar baz  qux  \")

See also: `sequence-split', `str:split', `split-sequence:split-sequence'"
  (check-type string string)
  (check-type char-bag (or character list))
  (check-type count (or null (integer 1)))
  (when max-num
    (warn "~S's ~S argument is deprecated; use ~S instead." 'string-split :max-num :count)
    (setf count max-num))
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
                count char-bag))))

(defun string-join* (strings &optional separator)
  "Join all non-nil elements of STRINGS together, separated by SEPARATOR.

Similar to `serapeum:string-join', but ignores nils in STRINGS.

Examples:

;; (string-join* (list \"foo\" \"bar\" \"baz\") \"-\") ;=> \"foo-bar-baz\"

;; (string-join* (list \"foo\" nil \"baz\") \"-\") ;=> \"foo-baz\"

See also: `concat', `serapeum:string-join'"
  (apply #'concatenate 'string
         (loop :for (item next) :on strings
               :if item
                 :collect item
               :if next
                 :collect separator)))

;; NOTE: shouldn't use this for long strings cuz it's not optimized
;; grabbed from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=)) ; FIX: rename to something else? string-replace-all is already taken in serapeum, but serapeum's implementation doesn't allow you to specify the character test function and/or case-sensitivity
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
  "Parse STRING as a boolean, returning either t or nil, or DEFAULT if it is not a known boolean string.

See also: `cl:parse-integer', `url-p'"
  (cond
    ((null string) default)
    ((member string (list "t" "1" "true" "y" "yes" "e" "enable" "enabled" "on") :test #'string-equal) t)
    ((member string (list "nil" "f" "0" "false" "n" "no" "d" "disable" "disabled" "off") :test #'string-equal) nil)
    (t default)))

(defun ip-vector-string (ip-vector)
  "Convert an IP specified as a 4-element sequence to an IP specified as a string.

See also: `ip-string-vector'"
  (when (stringp ip-vector)
    (return-from ip-vector-string ip-vector))
  (string-join* (mapcar #'write-to-string (coerce ip-vector 'list)) "."))

(defun ip-string-vector (ip-string)
  "Convert an IP specified as a string to an IP specified as a 4-element vector.

See also: `ip-vector-string'"
  (when (simple-vector-p ip-string)
    (return-from ip-string-vector ip-string))
  (coerce (mapcar #'parse-integer (string-split ip-string :char-bag #\.)) 'vector))

(defun url-p (string &key ignore-case)
  "True if STRING looks like a valid URL. If CASE-SENSITIVE, the protocol must be lowercase for the URL to be valid.

See also: `open-url', `pathname-designator-p', `parse-boolean'"
  (let ((split-up (string-split string :char-bag '(#\/))))
    (and (member (first split-up) (list "http:" "https:") :test (if ignore-case #'string-equal #'string=))
         (cdr split-up))))

(defun friendly-ratio-string (ratio &optional (separator " "))
  "Format a ratio as a more human-readable string.

Example:

;; (friendly-ratio-string 5/4) ;=> \"1 1/4\"
;; (friendly-ratio-string 9/7) ;=> \"1 2/7\"

See also: `friendly-duration-string'"
  (etypecase ratio
    (ratio
     (if (> (abs ratio) 1)
         (let* ((flr (funcall (if (plusp ratio) #'floor #'ceiling) ratio))
                (rem (- ratio flr)))
           (concat flr (unless (zerop rem)
                         (concat separator (abs rem)))))
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
              (format nil (if hourp "~2,'0D" "~D") min)
              (format nil ":~2,'0D" sec)
              (when include-ms
                (format nil ".~3,'0D" (truncate (* 1000 frac))))))))

(defun friendly-bytes (bytes &key short)
  "Given BYTES, a number of bytes, convert to the most \"friendly\" unit and return a list containing the number and the unit.

When SHORT is true, the unit is abbreviated.

See also: `friendly-bytes-string'"
  (let* ((idx (loop :for i :from 0 :upto 5
                    :if (< bytes (expt 1024 (1+ i)))
                      :return i))
         (num (/ bytes (expt 1024 idx))))
    (list num (concat (nth (+ (if short 0 6) idx)
                           (list "B" "KB" "MB" "GB" "TB" "PB"
                                 "Byte" "Kilobyte" "Megabyte" "Gigabyte" "Terabyte" "Petabyte"))
                      (when (and (not short) (/= 1 num)) "s")))))

(defun friendly-bytes-string (bytes &key short)
  "Generate a string describing BYTES as a number of bytes, kilobytes, megabytes, etc. When SHORT is true, the unit is abbreviated.

See also: `friendly-bytes'"
  (let ((fb (friendly-bytes bytes :short short)))
    (format nil "~$ ~A" (first fb) (second fb))))

;; FIX: ensure this works with Lisp's built-in indentation functionality?
(defun pretty-print-tree (tree &optional (indent 0))
  "Pretty print TREE, indenting the elements of each sublist."
  (dolist (e tree)
    (if (listp e)
        (pretty-print-tree e (1+ indent))
        (progn
          (dotimes (n indent)
            (princ "  "))
          (format t "- ~S~%" e)))))

;;; math

(defun approx= (number1 number2 &optional (max-dist 0.0001))
  "Test whether NUMBER1 and NUMBER2 are \"approximately\" equal, i.e. within MAX-DIST of each other.

See also: `near-zero-p'"
  (>= max-dist (abs (- number1 number2))))

(defun near-zero-p (number &optional (max-dist 0.0001))
  "True if NUMBER is within MAX-DIST of zero. Helps guard against division by zero.

See also: `approx='"
  (<= (- max-dist) number max-dist))

(defun wrap (number &optional (bottom 0) (top 1))
  "Wraps a number between BOTTOM and TOP, similar to `cl:mod'.

Examples:

;; (wrap 2 0 1) ; => 0
;; (wrap 5 0 10) ; => 5
;; (wrap 15 0 10) ; => 4

See also: `fold', `cl:mod', `alexandria:clamp'"
  (check-type number number)
  (check-type bottom number)
  (check-type top number)
  (+ (mod (- number bottom) (- top bottom)) bottom))

(defun fold (number &optional (bottom 0) (top 1))
  "Fold numbers outside BOTTOM and TOP back into the range.

Examples:

;; (fold -1 0 1) ;=> 1
;; (fold 5 0 10) ;=> 5
;; (fold 8 0 7) ;=> 6

See also: `wrap', `cl:mod', `alexandria:clamp'"
  (check-type number number)
  (check-type bottom number)
  (check-type top number)
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

;; (round-by 1 2) ; => 0
;; (round-by 1.1 0.5) ; => 1.0
;; (round-by 6 10) ; => 10

See also: `cl:round', `floor-by', `ceiling-by'"
  (* (round (/ number by)) by))

(uiop:with-deprecation (:error)
  (defun round-by-direction (number &optional (by 1))
    "Deprecated; use either `floor-by', `ceiling-by', or `round-by' instead."
    (if (zerop (mod number by))
        number
        (let* ((positive (plusp by))
               (diff (cadr (multiple-value-list (funcall (if positive #'floor #'ceiling) number (abs by))))))
          (funcall (if positive #'+ #'-) number (funcall (if positive #'- #'+) (abs by) diff))))))

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

(defun random-range.new (low &optional high) ; version 2, with support for ratios - FIX
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

(defun exponential-random-range (low high) ; adapted from supercollider/include/plugin_interface/SC_RGen.h
  "Generate a random number between LOW and HIGH, with exponential distribution.

See also: `random-range', `random-gauss'"
  (* low
     (exp (* (log (/ high
                     low))
             (random 1d0)))))

(defun random-gauss (mean standard-deviation)
  "Generate a random number from a normal (Gaussian) distribution.

See also: `random-range', `exponential-random-range', `alexandria:gaussian-random'"
  (let* ((first-random (random 1.0))
         (sqrt-result (if (equal first-random 0.0)
                          (sqrt most-positive-single-float)
                          (sqrt (* -2 (log first-random))))))
    (+ (* sqrt-result
          (sin (random (* 2 pi)))
          standard-deviation)
       mean)))

;;; lists and sequences

(uiop:with-deprecation (:style-warning)
  (defun length-upto (sequence &optional (max 10))
    "Deprecated function; use `alexandria:length=' instead.

Get the length of SEQUENCE, not counting above MAX.

Example:

;; (length-upto (make-list 200) 20) ;=> 20

See also: `alexandria:length='"
    (loop :with res := 0
          :for i :being :the :elements :of sequence
          :repeat max
          :do (incf res)
          :finally (return res))))

(uiop:with-deprecation (:warning)
  (defun list-length-upto (list &optional (max 10))
    "Deprecated function; use `alexandria:length=' instead."
    (length-upto list max)))

(uiop:with-deprecation (:style-warning)
  (defun list-length>= (list n)
    "Deprecated function; use `serapeum:length>=' or `alexandria:length=' instead.

True if LIST is at least N in length. Probably more efficient than doing something like (>= (length list) n).

Example:

;; (list-length>= (make-list 300) 10) ;=> T

See also: `serapeum:length>=', `alexandria:length='"
    (let ((current 0))
      (dolist (item list nil)
        (incf current)
        (when (>= current n)
          (return-from list-length>= t)))))

  (defun list-length> (list n)
    "Deprecated function; use `serapeum:length>' or `alexandria:length=' instead.

True if LIST is more than N in length.

See also: `serapeum:length>', `alexandria:length='"
    (list-length>= list (1+ n))))

(defun nth-wrap (n list)
  "Get the Nth item in LIST, wrapping the index if out of range. Returns the number of times \"wrapped\" as a second value.

Much like `nth', this function can only be used on lists. Use `elt-wrap' to index into any kind of sequence. However, keep in mind that `elt-wrap' may be slower when used on large lists.

See also: `elt-wrap'"
  (check-type n integer)
  (check-type list list)
  (multiple-value-bind (wraps mod) (floor n (list-length list))
    (values (nth mod list) wraps)))

(defun elt-wrap (sequence n)
  "Get the Nth item in SEQUENCE, wrapping the index if out of range. Returns the number of times \"wrapped\" as a second value.

Much like `elt', this function can be used on any sequence. However, because this function calls `length' to determine the wrapped index, it may be slow when used on large lists. Consider using `nth-wrap' in those cases instead.

See also: `nth-wrap'"
  (check-type n integer)
  (check-type sequence sequence)
  (multiple-value-bind (wraps mod) (floor n (length sequence))
    (values (elt sequence mod) wraps)))

(defun find-if* (predicate sequence) ; FIX: need to implement `find-if''s other arguments
  "Like `find-if', but return the index of the found item as a second value.

See also: `find-member'"
  (if (listp sequence)
      (let ((index 0))
        (dolist (item sequence)
          (when (funcall predicate item)
            (return-from find-if* (values item index)))
          (incf index)))
      (let ((length (length sequence)))
        (do ((index 0 (1+ index)))
            ((>= index length))
          (let ((item (elt sequence index)))
            (when (funcall predicate item)
              (return-from find-if* (values item index))))))))

(defun find-member (items list &key test)
  "Returns the first item from ITEMS that is a member of LIST, or nil if none are found.

See also: `find-if*'"
  (dolist (item items)
    (when (position item list :test test)
      (return-from find-member item))))

(defun most (function list &key (key #'identity)) ; from https://stackoverflow.com/questions/30273802/how-would-i-get-the-min-max-of-a-list-using-a-key
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

(defun flop (lists)
  "Given LISTS, a list of lists, swap rows for columns.

Example:

;; (flop '((0 1 2)
;;         (2 0 1)
;;         (1 2 0)))
;; ;=> ((0 2 1)
;;      (1 0 2)
;;      (2 1 0))"
  (loop :repeat (reduce #'max lists :key #'length)
        :for i :from 0
        :collect (mapcar (lambda (list)
                           (nth-wrap i list))
                         lists)))

(defun subseq* (sequence start &optional end) ; FIX: name conflicts with alexandria-2:subseq*
  "Like subseq, but allows start and end to be negative."
  (let ((length (length sequence)))
    (subseq sequence (if (< start 0)
                         (+ length start)
                         start)
            (if (and (numberp end) (< end 0))
                (+ length end)
                end))))

(defun repeat (item repeats) ; FIX: rename to `make-list+' ?
  "Get a list containing REPEATS ITEMs. If ITEM is a function, return a list of REPEATS of the result of that function.

Example:

;; (repeat (lambda () (random 10)) 10)
;; ;=> (7 0 6 6 7 9 8 1 9 8)"
  (check-type repeats (integer 0))
  (the list
       (when (plusp repeats)
         (cons (if (typep item 'function)
                   (funcall item)
                   item)
               (repeat item (- repeats 1))))))

(defun left-trim (bag sequence &key (test #'eql))
  "Trim anything from BAG from the start of SEQUENCE.

See also: `list-left-trim', `cl:string-left-trim'"
  (let* ((bag (if (typep bag 'sequence) bag (list bag)))
         (pos (position-if-not (lambda (x) (find x bag :test test)) sequence)))
    (subseq sequence (or pos (length sequence)))))

(defun list-left-trim (bag list &key (test #'eql))
  "Trim anything from BAG from the start of LIST.

 See also: `left-trim', `cl:string-left-trim'"
  (let ((bag (ensure-list bag)))
    (member-if-not (lambda (x) (position x bag :test test)) list)))

(uiop:with-deprecation (:warning)
  (defmacro affixnew (place thing)
    "Affix THING to the end of PLACE if it's not already a member.

See also: `alexandria:appendf', `cl:pushnew'."
    (once-only (thing)
      `(unless (position ,thing ,place)
         (appendf ,place (list ,thing)))))

  (defun split-sequence (sequence delimiter) ; conflicts with `split-sequence:split-sequence' which is used by serapeum; use sequence-split instead.
    "Deprecated alias for `sequence-split'."
    (sequence-split sequence delimiter)))

(defun sequence-split (sequence delimiter &key (test #'position) (offset (if (typep delimiter 'sequence) (length delimiter) 1)))
  "Split SEQUENCE by searching for instances of DELIMITER using TEST. After finding a match for DELIMITER, the next search is run on the subsequence of SEQUENCE OFFSET from the location of the match.

Example:

;; (sequence-split \"foo - bar - baz\" \" - \" :test 'search)
;; ;=> (\"foo\" \"bar\" \"baz\")

See also: `sequence-replace', `balanced-subsequences'"
  (if-let ((pos (funcall test delimiter sequence)))
    (cons (subseq sequence 0 pos)
          (sequence-split (subseq sequence (+ offset pos)) delimiter :test test :offset offset))
    (cons sequence nil)))

(defun sequence-replace (sequence target replacement &key (test #'eql) count (key #'identity))
  "Replace instances of TARGET with REPLACEMENT in SEQUENCE, optionally limiting to COUNT replacements. Returns the number of replacements made as a second value.

See also: `sequence-split'"
  (loop :with replacements := 0
        :for element :being :the :elements :of sequence
        :if (and (funcall test target (funcall key element))
                 (or (null count)
                     (< replacements count)))
          :collect (progn
                     (incf replacements)
                     replacement)
            :into replaced
        :else
          :collect element :into replaced
        :finally (return (values replaced replacements))))

;; FIX: remove the requirement that OPEN and CLOSE differ, and add an ESCAPE argument to denote a character used for escaping OPEN/CLOSE
(defun balanced-subsequences (sequence &key (open #\[) (close #\]) (test #'char=))
  "Get a list of subsequences of SEQUENCE enclosed between a balanced pairs of OPEN and CLOSE.

Example:

;; (balanced-subsequences \"foo [bar] baz [qux [this]] that\" :open #\[ :close #\] :test #'char=)
;; ;=> (\"bar\" \"qux [this]\")

See also: `sequence-split', `cl:read'"
  (assert (not (funcall test open close)) (open close test) "OPEN and CLOSE may not be equal according to TEST.")
  (loop :with start
        :with balance := 0
        :for idx :from 0
        :for char :being :the :elements :of sequence
        :for open-p := (funcall test char open)
        :for close-p := (funcall test char close)
        :if open-p
          :do (when (zerop balance)
                (setf start idx))
              (setf balance (1+ balance))
        :if close-p
          :do (decf balance)
        :if (and close-p (zerop balance))
          :collect (subseq sequence (1+ start) idx)))

(uiop:with-deprecation (:style-warning)
  (defun insert-if (function list item) ; FIX: remove. this is only used cl-patterns' eseq class functionality.
    "Destructively insert ITEM into LIST at the position where FUNCTION is true. If the function doesn't return true, the item is inserted at the end of the list. Similar to `nreverse', the input list is destructively modified.

Example:

;; (insert-if #'plusp (list -2 -1 1 2) 0) ;=> (-2 -1 0 1 2)"
    (unless list
      (return-from insert-if (list item)))
    (loop :for i :on list
          :if (funcall function (car i))
            :do (setf (cdr i) (cons (car i) (cdr i))
                      (car i) item)
                (loop-finish)
          :unless (cdr i)
            :do (setf (cdr i) (cons item nil))
                (loop-finish)
          :finally (return list))))

;;; functions

(defun funcallable-object-p (object)
  "True if OBJECT is a funcallable object."
  (closer-mop:subclassp (class-of (class-of object)) 'closer-mop:funcallable-standard-class))

(deftype function-designator ()
  "An object that can be used to designate a function, i.e. a function, an `fboundp' symbol, or a funcallable object."
  '(or function (satisfies funcallable-object-p) (and symbol (satisfies fboundp))))

(defun function-designator-p (object)
  "True if OBJECT is a `function-designator', i.e. a string or pathname."
  (typep object 'function-designator))

;;; "operator adverbs"
;; inspired by SuperCollider's concept of the same name; https://doc.sccode.org/Reference/Adverbs.html

(declaim (inline mapshort))

(defun mapshort (function &rest lists)
  "\"Short\" operator adverb; apply FUNCTION to successive sets of elements from LISTS. Mostly here for completion sake, as this is effectively the same thing as regular `mapcar'.

This function is inspired by and equivalent to SuperCollider's \".s\" operator adverb.

Example:

;; (mapshort #'+ (list 10 20 30 40 50) (list 1 2 3))
;; => (11 22 33)

See also: `mapwrap', `mapfold', `maptable', `mapcross'"
  (apply #'mapcar function lists))

(defun mapwrap (function &rest lists)
  "\"Wrap\" operator adverb; apply FUNCTION to successive sets of elements from LISTS, producing a list that is the length of the longest list by \"wrapping\" indexes into the shorter lists.

This is similar to `mapcar' but results in a list that is the length of the longest input list.

This function is inspired by and equivalent to SuperCollider's \".w\" operator adverb.

Example:

;; (mapwrap #'+ (list 10 20 30 40 50) (list 1 2 3))
;; => (11 22 33 41 52)

See also: `mapshort', `mapfold', `maptable', `mapcross'"
  (let (more-values)
    (apply #'values
           (loop :for i :from 0 :below (reduce #'max (mapcar #'length lists))
                 :for res := (multiple-value-list
                              (apply function (mapcar (lambda (list)
                                                        (elt-wrap list i))
                                                      lists)))
                 :collect (car res)
                 :do (setf more-values (cdr res)))
           more-values)))

(defun mapfold (function &rest lists)
  "\"Fold\" operator adverb; apply FUNCTION to successive sets of elements from LISTS, producing a list that is the length of the longest list by \"folding\" indexes into the shorter lists.

This is similar to `mapcar' but results in a list that is the length of the longest input list.

This function is inspired by and equivalent to SuperCollider's \".f\" operator adverb.

Example:

;; (mapfold #'+ (list 10 20 30 40 50) (list 1 2 3))
;; ;=> (11 22 33 42 51)

See also: `mapshort', `mapwrap', `maptable', `mapcross'"
  (let (more-values
        (lengths (mapcar #'length lists)))
    (apply #'values
           (loop :for i :from 0 :below (reduce #'max lengths)
                 :for res := (multiple-value-list
                              (apply function (loop :for list :in lists
                                                    :for length :in lengths
                                                    :collect (elt list (fold i 0 (1- length))))))
                 :collect (car res)
                 :do (setf more-values (cdr res)))
           more-values)))

(defun maptable (function &rest lists)
  "\"Table\" operator adverb; apply FUNCTION to each element of each list in LISTS, producing a list of lists for each.

This function is inspired by and equivalent to SuperCollider's \".t\" operator adverb.

Example:

;; (maptable #'+ (list 10 20 30 40 50) (list 1 2 3))
;; ;=> ((11 12 13) (21 22 23) (31 32 33) (41 42 43) (51 52 53))

See also: `mapshort', `mapwrap', `mapfold', `mapcross'"
  (let ((res (car lists)))
    (dolist (list (cdr lists) res)
      (setf res (mapcar (lambda (elem)
                          (mapcar (curry function elem) list))
                        res)))))

(defun mapcross (function &rest lists)
  "\"Cross\" operator adverb; similar to `maptable' but results in a flat list.

This function is inspired by and equivalent to SuperCollider's \".x\" operator adverb.

Example:

;; (maptable #'+ (list 10 20 30 40 50) (list 1 2 3))
;; ;=> (11 12 13 21 22 23 31 32 33 41 42 43 51 52 53)

See also: `mapshort', `mapwrap', `mapfold', `maptable'"
  (let ((res (car lists)))
    (dolist (list (cdr lists) res)
      (setf res (mapcan (lambda (elem)
                          (mapcar (curry function elem) list))
                        res)))))

;;; hash tables

(defun save-hash-table (hash filename &key (if-exists :error))
  "Save a hash table to a file. See `restore-hash-table' to load the saved table.

Example:

;; (save-hash-table *my-hash* \"/home/user/blah.hash\" :if-exists :rename)

See also: `restore-hash-table'"
  (with-open-file (stream filename :direction :output :if-exists if-exists)
    (princ "(" stream)
    (let ((*print-readably* t))
      (maphash (lambda (key value)
                 (print (cons key value) stream))
               hash))
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

;;; CLOS

(defun all-classes (&optional package)
  "Get a list of all defined classes in the Lisp image. With PACKAGE, only get classes belonging to that package.

See also: `subclasses-of'"
  (let ((package (if (or (null package)
                         (eql package t))
                     nil
                     (or (find-package package)
                         (error "Package ~S not found." package))))
        (classes (subclasses-of t)))
    (if package
        (remove-if-not (fn (eql (symbol-package (class-name _)) package))
                       classes)
        classes)))

(defun subclasses-of (class &key recursive-p)
  "Get a list of all direct subclasses of CLASS. If RECURSIVE-P is true, recursively get all subclasses.

See also: `all-classes'"
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class)))))
    (labels ((direct-subclasses-of (class)
               (closer-mop:class-direct-subclasses class))
             (subclasses-recursive (class)
               (mappend (fn (list _ (subclasses-recursive _)))
                        (direct-subclasses-of class))))
      (remove-duplicates (flatten (funcall (if recursive-p
                                               #'subclasses-recursive
                                               #'direct-subclasses-of)
                                           class))))))

(deftype slot-definition-slot ()
  "Slots of slot definitions. This is primarily used for `find-class-slot'.

See also: `find-class-slot'"
  '(member
    :allocation :location
    :name
    :initarg :initargs
    :initform
    :initfunction
    :accessor :accessors
    :reader :readers
    :writer :writers
    :type
    :documentation))

(defun find-class-slot (class key value &key test)
  "Find a slot in CLASS whose slot option KEY is true when TESTed against VALUE."
  (check-type key slot-definition-slot)
  (let* ((class (etypecase class
                  (standard-class class)
                  (symbol (find-class class))))
         (slots (append (closer-mop:class-direct-slots class)
                        (closer-mop:class-slots class)))
         (accessor (case key
                     (:documentation (lambda (slot) (documentation slot t)))
                     (t (intern (concat "SLOT-DEFINITION-" (case key
                                                             (:initarg :initargs)
                                                             ((:accessor :accessors :reader) :readers)
                                                             (:writer :writers)
                                                             (t key)))
                                'closer-mop)))))
    (find value slots :key accessor
                      :test (or test
                                (case key
                                  ((:initarg :accessor :reader :writer) #'find)
                                  (:documentation #'string=)
                                  (t #'eql))))))

;;; introspection

(defun lisp-uptime ()
  "Get the number of seconds that Lisp has been running for."
  (/ (get-internal-real-time) internal-time-units-per-second))

(uiop:with-deprecation (:style-warning)
  (defun current-seconds ()
    "Deprecated alias for `lisp-uptime'."
    (lisp-uptime)))

;; swiped from https://stackoverflow.com/questions/15465138/find-functions-arity-in-common-lisp
(defun function-arglist (function) ; FIX: maybe just use trivial-arguments instead? ; FIX: rename to function-lambda-list for consistency with alexandria?
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

(defun systems-depending-on (system) ; see also: deptree; https://github.com/varjagg/deptree
  "Get a list of systems in the current image that include SYSTEM in their :depends-on."
  (let (systems)
    (asdf:map-systems
     (fn (when (position system (asdf:system-depends-on _)
                         :test (lambda (x y)
                                 (if (every #'string-designator-p (list x y))
                                     (string-equal x y)
                                     (equalp x y))))
           (push _ systems))))
    systems))

(defun lisp-connections ()
  "Get a list of the current connections to this Lisp image."
  (when-let ((package (cond ((find-package 'slynk) 'slynk)
                            ((find-package 'swank) 'swank))))
    (symbol-value (find-symbol "*CONNECTIONS*" package))))

;;; files

(deftype pathname-designator ()
  "An object that can be used to designate a pathname, i.e. a string or pathname."
  '(or string pathname))

(defun pathname-designator-p (object)
  "True if OBJECT is a `pathname-designator', i.e. a string or pathname."
  (typep object 'pathname-designator))

(defun join-path-components (&rest path-components)
  "Join PATH-COMPONENTS together into a single string, ensuring each is separated by exactly one directory separator.

Example:

;; (join-path-components \"foo\" \"/bar\" \"baz.qux) ;=> \"foo/bar/baz.qux\"

See also: `cl:merge-pathnames', `uiop:merge-pathnames*'"
  (let ((sep (uiop:directory-separator-for-host)))
    (labels ((joiner (compo &optional first)
               (concat (funcall (curry (if first
                                           #'string-right-trim
                                           (if (cdr compo)
                                               #'string-trim
                                               #'string-left-trim))
                                       (list sep))
                                (namestring (car compo)))
                       (when (cdr compo)
                         sep)
                       (when (cdr compo)
                         (joiner (cdr compo))))))
      (joiner path-components t))))

(uiop:with-deprecation (:warning)
  (defun join-pathnames (&rest filenames)
    "Deprecated alias for `join-path-components'."
    (apply 'join-path-components filenames)))

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
;; => \"/tmp/lisp/2020-04-20-06-09-00.flac\"

See also: `uiop:tmpize-pathname', `uiop:temporary-directory'"
  (uiop:ensure-pathname directory :ensure-directories-exist t)
  (let* ((name (or name
                   (local-time:format-timestring nil (local-time:now) ; FIX: make the local-time dependency optional?
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
                  (string-downcase extension))
                 (string extension)))
         (attempt 0))
    (flet ((gen-filename (attempt)
             (make-pathname :directory (pathname-directory directory)
                            :name (format nil "~A~@[-~3,'0D~]" name (unless (zerop attempt) attempt))
                            :type type)))
      (loop :while (uiop:file-exists-p (gen-filename attempt))
            :do (incf attempt))
      (namestring (gen-filename attempt)))))

;; conditionally load emacs-extensions if swank or slynk are available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (or (featurep :swank)
            (featurep :slynk))
    (load (asdf:system-relative-pathname :mutility "src/extensions/emacs-extensions.lisp"))))
