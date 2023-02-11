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

   #:concat
   #:output
   #:vowel-char-p
   #:string-designator-p
   #:split-string
   #:string-split
   #:string-join*
   #:replace-all
   #:parse-boolean
   #:url-p
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
   #:round-by-direction ; deprecated

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
   #:find-any
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

   #:save-hash-table
   #:restore-hash-table

   #:all-classes
   #:subclasses-of

   #:current-seconds
   #:function-arglist
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
   #:ringbuffer-oldest
   #:do-ringbuffer))
