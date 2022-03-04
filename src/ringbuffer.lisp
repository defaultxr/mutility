(in-package #:mutility)

;;;; ringbuffer.lisp - ringbuffer implementation.
;;; https://en.wikipedia.org/wiki/Circular_buffer

(defstruct (ringbuffer (:constructor %make-ringbuffer))
  "A ringbuffer, also known as a circular buffer. Items can be pushed onto it and popped from it just like a regular stack, except that has a finite size. After SIZE elements are pushed to it, the next push overwrites the least recent element."
  (size 10 :type integer :read-only t)
  (index 0 :type integer)
  (length 0 :type integer)
  (initial-element nil)
  array)

(setf (documentation 'ringbuffer-size 'function) "The maximum size of the ringbuffer.

See also: `ringbuffer-length', `ringbuffer-index', `ringbuffer-initial-element'")

(setf (documentation 'ringbuffer-index 'function) "The current index into the ringbuffer that new elements will be pushed to.

See also: `ringbuffer-length', `ringbuffer-size', `ringbuffer-initial-element'")

(setf (documentation 'ringbuffer-length 'function) "The length of the ringbuffer, i.e. the number of items currently in it. It is always a number in the range from 0 to `ringbuffer-size'.

See also: `ringbuffer-index', `ringbuffer-size', `ringbuffer-initial-element'")

(setf (documentation 'ringbuffer-initial-element 'function) "The initial element that each cell in the ringbuffer defaults to, and is set to when the cell is `ringbuffer-pop'ped or `ringbuffer-get'ed.

See also: `ringbuffer-size', `ringbuffer-index', `ringbuffer-length'")

(setf (documentation 'ringbuffer-array 'function) "The actual array object that contains the ringbuffer data.

See also: `ringbuffer-size', `ringbuffer-index', `ringbuffer-length', `ringbuffer-initial-element'")

(defun make-ringbuffer (size &optional initial-element)
  "Make a `ringbuffer' object of the specified size and the specified initial element.

See also: `ringbuffer-size', `ringbuffer-index', `ringbuffer-initial-element', `ringbuffer-push', `ringbuffer-elt', `ringbuffer-pop', `ringbuffer-get', `ringbuffer'"
  (%make-ringbuffer :size size
                    :initial-element initial-element
                    :array (make-array size :initial-element initial-element)))

(defun ringbuffer-elt (ringbuffer &optional (index -1))
  "Get the element at INDEX in RINGBUFFER. Defaults to the most recent element pushed.

See also: `ringbuffer-get', `ringbuffer-push', `ringbuffer-size', `ringbuffer-index', `ringbuffer-initial-element', `ringbuffer'"
  (aref (ringbuffer-array ringbuffer) (mod (+ (ringbuffer-index ringbuffer)
                                              index)
                                           (ringbuffer-size ringbuffer))))

(defun (setf ringbuffer-elt) (value ringbuffer &optional (index -1))
  (setf (aref (ringbuffer-array ringbuffer) (mod (+ (ringbuffer-index ringbuffer)
                                                    index)
                                                 (ringbuffer-size ringbuffer)))
        value))

(defun ringbuffer-push (ringbuffer &optional (object (ringbuffer-initial-element ringbuffer)))
  "Add OBJECT to RINGBUFFER.

See also: `ringbuffer-pop', `ringbuffer-get', `ringbuffer-elt', `ringbuffer-size', `ringbuffer-index', `ringbuffer-initial-element', `ringbuffer'"
  (let ((index (ringbuffer-index ringbuffer)))
    (setf (ringbuffer-elt ringbuffer 0) object
          (ringbuffer-index ringbuffer) (mod (1+ index) (ringbuffer-size ringbuffer))
          (ringbuffer-length ringbuffer) (min (1+ (ringbuffer-length ringbuffer)) (ringbuffer-size ringbuffer)))))

(defun ringbuffer-pop (ringbuffer)
  "Get the element most recently pushed to RINGBUFFER, removing it and decreasing the `ringbuffer-index' to point at the next most recent element.

See also: `ringbuffer-get', `ringbuffer-elt', `ringbuffer-push', `ringbuffer-size', `ringbuffer-index', `ringbuffer-initial-element', `ringbuffer'"
  (let ((length (ringbuffer-length ringbuffer)))
    (prog1 (ringbuffer-elt ringbuffer -1)
      (setf (ringbuffer-elt ringbuffer -1) (ringbuffer-initial-element ringbuffer)
            (ringbuffer-index ringbuffer) (mod (1- (ringbuffer-index ringbuffer)) (ringbuffer-size ringbuffer))
            (ringbuffer-length ringbuffer) (max 0 (1- length))))))

(defun ringbuffer-get (ringbuffer)
  "Get the oldest element from RINGBUFFER, removing it in the process.

See also: `ringbuffer-pop', `ringbuffer-elt', `ringbuffer-push', `ringbuffer-size', `ringbuffer-index', `ringbuffer-initial-element', `ringbuffer'"
  (let* ((length (ringbuffer-length ringbuffer))
         (oldest-index (mod (- (ringbuffer-index ringbuffer) length) (ringbuffer-size ringbuffer))))
    (prog1 (ringbuffer-elt ringbuffer oldest-index)
      (setf (ringbuffer-elt ringbuffer oldest-index) (ringbuffer-initial-element ringbuffer)
            (ringbuffer-length ringbuffer) (max 0 (1- length))))))

(defmacro do-ringbuffer ((var ringbuffer &optional result-form) &body body)
  "Execute BODY once for each element in RINGBUFFER from least to most recent, with VAR bound to the element, returning RESULT-FORM.

See also: `ringbuffer-elt', `ringbuffer-push', `ringbuffer-pop', `ringbuffer-size', `ringbuffer-index', `ringbuffer-length', `ringbuffer'"
  (with-gensyms (ringbuffer-sym idx-sym length-sym)
    `(let* ((,ringbuffer-sym ,ringbuffer)
            (,idx-sym (ringbuffer-index ,ringbuffer-sym))
            (,length-sym (ringbuffer-length ,ringbuffer-sym)))
       (dotimes (idx ,length-sym ,result-form)
         (let ((,var (ringbuffer-elt ,ringbuffer-sym (- idx ,length-sym))))
           ,@body)))))