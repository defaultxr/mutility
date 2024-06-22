;;;; queue.lisp - queue implementation.
;;; https://en.wikipedia.org/wiki/Circular_buffer

(in-package #:mutility)

(defstruct (queue (:constructor %make-queue))
  "A first-in-first-out queue.

See also: `ringbuffer'"
  (length 0 :type (integer 0))
  (index 0 :type integer)
  (array #() :type array))

(setf (documentation 'queue-length 'function) "The number of items currently in the queue.

See also: `queue-index', `queue-size', `queue'")

(setf (documentation 'queue-index 'function) "The index into the queue's internal array of the element that would be dequeued by `queue-dequeue'.

See also: `queue-length', `queue-size', `queue-dequeue', `queue'")

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (length index) queue
      (format stream "~S ~S ~S ~S ~S ~S" :size (queue-size queue) :length length :contents (queue-contents queue)))))

(defun make-queue (size)
  "Make a `queue' object of the specified size and the specified initial element.

See also: `queue-size', `queue-enqueue', `queue-dequeue', `queue-elt', `queue'"
  (%make-queue :array (make-array size)))

(defun queue-size (queue)
  "The maximum number of elements that QUEUE can hold."
  (length (queue-array queue)))

(defun queue-contents (queue)
  "Get a list of the current contents of QUEUE."
  (loop :for idx :below (queue-length queue)
        :collect (aref (queue-array queue) (mod (+ idx (queue-index queue)) (queue-size queue)))))

(defun (setf queue-contents) (queue contents)
  "Set QUEUE's contents to CONTENTS."
  (loop :for idx :from 0
        :for item :in contents
        :do (setf (aref (queue-array queue) idx) item)
        :finally (setf (queue-index queue) 0
                       (queue-length queue) (length contents))))

(define-condition queue-full ()
  ((queue :initarg :queue :reader queue-full-queue))
  (:report (lambda (condition stream)
             (format stream "Queue ~S is full." (queue-full-queue condition))))
  (:documentation "Condition for when a `queue' is full."))

(defun queue-full-p (queue)
  "True when QUEUE is full.

See also: `queue-enqueue', `queue-empty-p', `queue-dequeue', `queue'"
  (= (queue-length queue) (queue-size queue)))

(defun queue-enqueue (queue object)
  "Add OBJECT to QUEUE. Signals `queue-full' if QUEUE is full.

See also: `queue-dequeue', `queue-contents', `queue'"
  (when (queue-full-p queue)
    (error 'queue-full :queue queue))
  (setf (aref (queue-array queue)
              (mod (+ (queue-length queue) (queue-index queue))
                   (queue-size queue)))
        object)
  (incf (queue-length queue)))

(define-condition queue-empty ()
  ((queue :initarg :queue :reader queue-empty-queue))
  (:report (lambda (condition stream)
             (format stream "Queue ~S is empty." (queue-empty-queue condition))))
  (:documentation "Condition for when a `queue' is empty."))

(defun queue-empty-p (queue)
  "True when QUEUE is empty.

See also: `queue-dequeue', `queue-full-p', `queue-enqueue', `queue'"
  (zerop (queue-length queue)))

(defun queue-dequeue (queue)
  "Get and remove the oldest item from QUEUE. Signals `queue-empty' if there are no elements in QUEUE.

See also: `queue-peek', `queue-enqueue', `queue-contents', `queue'"
  (when (queue-empty-p queue)
    (error 'queue-empty :queue queue))
  (prog1 (aref (queue-array queue) (queue-index queue))
    (setf (queue-index queue) (mod (+ 1 (queue-index queue)) (queue-size queue))
          (queue-length queue) (1- (queue-length queue)))))

(defun queue-peek (queue)
  "Get the oldest item from QUEUE. Signals `queue-empty' if there are no elements in QUEUE.

See also: `queue-dequeue', `queue-enqueue', `queue-contents', `queue'"
  (when (queue-empty-p queue)
    (error 'queue-empty :queue queue))
  (aref (queue-array queue) (queue-index queue)))

(define-condition queue-index-out-of-range ()
  ((queue :initarg :queue :reader queue-index-out-of-range-queue)
   (index :initarg :index :reader queue-index-out-of-range-index))
  (:report (lambda (condition stream)
             (format stream "Index ~S of ~S is out of range." (queue-index-out-of-range-index condition) (queue-index-out-of-range-queue condition))))
  (:documentation "Condition for when the requested index of a `queue' is out of range."))

(defun queue-elt-internal-index (queue index)
  "Get the index into QUEUE's internal array to access INDEX.

See also: `queue-elt'"
  (mod (if (minusp index)
           (+ index (queue-index queue) (queue-length queue))
           (+ index (queue-index queue)))
       (queue-size queue)))

(defun queue-elt (queue index)
  "Get the element at INDEX in QUEUE. Negative indexes are from the most recently-pushed elements, while non-negative are from the oldest. So -1 is the most recently-queued item, and -2 is the second most. 0 is the oldest item in the queue, and 1 is the second oldest.

Examples:

;; (defparameter q (make-queue 3))
;; (queue-enqueue q 0)
;; (queue-enqueue q 1)
;; (queue-enqueue q 2)
;; ;; Get the most recently-queued element:
;; (queue-elt q -1) ;=> 2
;; ;; Get the oldest element:
;; (queue-elt q 0) ;=> 0
;; ;; Get the second-oldest element:
;; (queue-elt q 1) ;=> 1

See also: `queue-enqueue', `queue-dequeue', `queue-size', `queue'"
  (let ((length (queue-length queue)))
    (when (or (>= index length)
              (< index (- length)))
      (error 'queue-index-out-of-range :queue queue :index index)))
  (aref (queue-array queue) (queue-elt-internal-index queue index)))

(defun (setf queue-elt) (value queue index)
  (setf (aref (queue-array queue) (queue-elt-internal-index queue index)) value))

(defmacro do-queue ((var queue &optional result-form) &body body)
  "Execute BODY once for each element in QUEUE from oldest to newest, with VAR bound to the element, then return RESULT-FORM.

See also: `queue-elt', `queue-enqueue', `queue-dequeue', `queue'"
  (with-gensyms (queue-sym)
    `(let* ((,queue-sym ,queue))
       (dotimes (idx (queue-length ,queue-sym) ,result-form)
         (let ((,var (queue-elt ,queue-sym idx)))
           ,@body)))))
