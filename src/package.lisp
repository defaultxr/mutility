;;;; package.lisp

(uiop:define-package #:mutility
  (:use #:cl
        #:alexandria)
  (:export
   :a
   :fn
   :cut
   :with-access
   :no-dictionary-entry
   :no-dictionary-entry-entry
   :no-dictionary-entry-dictionary-name
   :no-dictionary-entry-dictionary
   :define-dictionary

   :dprint

   :keys

   :upcase-intern
   :my-intern
   :reintern
   :un-intern

   :friendly-string
   :friendly-symbol

   :concat
   :output
   :vowel-char-p
   :string-designator-p
   :split-string
   :replace-all
   :parse-boolean
   :friendly-ratio-string
   :friendly-duration-string
   :pretty-print-tree

   :wrap
   :fold
   :floor-by
   :ceiling-by
   :round-by
   :round-by-direction

   :list-length-upto
   :length-upto
   :list-length>=
   :list-length>
   :nth-wrap
   :elt-wrap
   :find-if*
   :find-any
   :most
   :flatten-1
   :subseq*
   ;; :repeat ;; conflicts with iterate
   :split-sequence
   :left-trim
   :affixnew
   :insert-if
   :insert-sorted

   :random-coin
   :random-range
   :exponential-random-range
   :random-gauss

   :save-hash-table
   :restore-hash-table

   :current-seconds
   :function-arglist
   :lisp-connections
   
   :open-url
   :generate-temporary-file-name
   :locate-dominating-file))
