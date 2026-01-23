;;;; package.lisp - definition for the mutility package.

(uiop:define-package #:mutility
  (:use #:cl
        #:alexandria)
  (:export
   ;; mutility.lisp
   #:a
   #:fn
   #:cut
   #:with-access ; deprecated
   #:defclass+
   #:no-dictionary-entry
   #:no-dictionary-entry-entry
   #:no-dictionary-entry-dictionary-name
   #:no-dictionary-entry-dictionary
   #:define-dictionary

   #:dprint

   #:keys

   #:upcase-intern

   ;; deprecated
   #:my-intern
   #:reintern
   #:un-intern

   #:friendly-string
   #:friendly-symbol

   #:+whitespace-chars+
   #:concat
   #:output
   #:numeric-char-p
   #:vowel-char-p
   #:string-designator-p
   #:split-string ; deprecated
   #:string-split
   #:string-split-by-string
   #:string-split-by-predicates
   #:string-join*
   #:string-replace-all*
   #:replace-all ; deprecated
   #:parse-boolean
   #:parse-number-and-string
   #:parse-friendly-bytes-string
   #:read-as-tokens
   #:ip-vector-string
   #:ip-string-vector
   #:url-p
   #:friendly-ratio-string
   #:friendly-duration-string
   #:friendly-bytes
   #:friendly-bytes-string
   #:pretty-print-tree
   #:rot13

   #:pow2
   #:approx=
   #:sequence-approx=
   #:near-zero-p
   #:wrap
   #:fold
   #:floor-by
   #:ceiling-by
   #:round-by

   #:random-coin
   #:random-range
   #:exponential-random-range
   #:random-gauss

   ;; deprecated
   #:length-upto
   #:list-length-upto
   #:list-length>=
   #:list-length>

   #:nth-wrap
   #:elt-wrap
   #:find-if*
   #:find-member
   #:most
   #:flatten-1
   #:flop
   #:subseq*
   ;; #:repeat ; conflicts with iterate
   #:left-trim
   #:list-left-trim
   #:affixnew ; deprecated
   #:split-sequence ; deprecated
   #:sequence-split
   #:sequence-replace
   #:balanced-subsequences
   #:insert-if ; deprecated

   #:funcallable-object-p
   #:function-designator
   #:function-designator-p
   #:ensure-funcall

   #:mapshort
   #:mapwrap
   #:mapfold
   #:maptable
   #:mapcross

   #:hash-table-save
   #:hash-table-restore
   #:save-hash-table ; deprecated
   #:restore-hash-table ; deprecated

   #:all-classes
   #:subclasses-of
   #:find-class-slot
   #:set-accessor-documentation-from-slots

   #:auto-load-systems

   #:lisp-uptime
   #:current-seconds ; deprecated
   #:function-arglist
   #:systems-depending-on
   #:lisp-connections

   #:pathname-designator
   #:pathname-designator-p
   #:join-path-components
   #:join-pathnames ; deprecated
   #:open-url
   #:generate-temporary-file-name

   ;; ringbuffer.lisp
   #:ringbuffer
   #:ringbuffer-size
   #:ringbuffer-index
   #:ringbuffer-length
   #:ringbuffer-initial-element
   #:ringbuffer-array
   #:make-ringbuffer
   #:ringbuffer-elt
   #:ringbuffer-push
   #:ringbuffer-pop
   #:ringbuffer-get
   #:ringbuffer-newest
   #:ringbuffer-from-newest
   #:ringbuffer-oldest
   #:ringbuffer-from-oldest
   #:do-ringbuffer

   ;; queue.lisp
   #:queue
   #:queue-length
   #:make-queue
   #:queue-size
   #:queue-contents
   #:queue-full
   #:queue-full-p
   #:queue-enqueue
   #:queue-empty
   #:queue-empty-p
   #:queue-dequeue
   #:queue-peek
   #:queue-index-out-of-range
   #:queue-elt
   #:do-queue))
