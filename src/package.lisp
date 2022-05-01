;;;; package.lisp

(uiop:define-package #:mutility
  (:use #:cl
        #:alexandria)
  (:export
   ;; mutility.lisp
   #:a
   #:fn
   #:cut
   #:with-access
   #:defclass+
   #:no-dictionary-entry
   #:no-dictionary-entry-entry
   #:no-dictionary-entry-dictionary-name
   #:no-dictionary-entry-dictionary
   #:define-dictionary

   #:dprint

   #:keys

   #:upcase-intern
   #:my-intern
   #:reintern
   #:un-intern

   #:friendly-string
   #:friendly-symbol

   #:concat
   #:output
   #:vowel-char-p
   #:string-designator-p
   #:split-string
   #:string-split
   #:replace-all
   #:parse-boolean
   #:friendly-ratio-string
   #:friendly-duration-string
   #:pretty-print-tree

   #:approx=
   #:near-zero-p
   #:wrap
   #:fold
   #:floor-by
   #:ceiling-by
   #:round-by
   #:round-by-direction

   #:random-coin
   #:random-range
   #:exponential-random-range
   #:random-gauss

   #:length-upto
   #:list-length-upto
   #:list-length>=
   #:list-length>
   #:nth-wrap
   #:elt-wrap
   #:find-if*
   #:find-any
   #:most
   #:flatten-1
   #:subseq*
   ;; #:repeat ;; conflicts with iterate
   #:left-trim
   #:affixnew
   #:split-sequence
   #:sequence-split
   #:sequence-replace
   #:insert-if

   #:funcallable-object-p
   #:function-designator
   #:function-designator-p

   #:save-hash-table
   #:restore-hash-table

   #:all-classes
   #:subclasses-of

   #:current-seconds
   #:function-arglist
   #:lisp-connections
   
   #:pathname-designator
   #:pathname-designator-p
   #:open-url
   #:generate-temporary-file-name
   #:locate-dominating-file

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
   #:do-ringbuffer))
