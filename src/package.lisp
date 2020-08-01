;;;; package.lisp

(defpackage #:mutility
  (:use #:cl
        #:alexandria)
  (:export
   :a
   :fn
   :multiple-value-elt
   :with-access

   :define-obsolete-function-alias

   :keys

   :friendly-symbol

   :concat
   :output
   :split-string
   :replace-all
   :parse-boolean
   :friendly-ratio-string
   :friendly-duration-string

   :my-intern
   :un-intern

   :wrap
   :floor-by
   :ceiling-by
   :round-by
   :round-by-direction

   :length-upto
   :nth-wrap
   :elt-wrap
   :find-any
   :most
   :flatten-1
   :subseq*
   ;; :repeat ;; conflicts with iterate
   :split-sequence
   :left-trim
   :insert-if
   :insert-sorted

   :random-coin
   :random-range
   :exponential-random-range
   :random-gauss

   :save-hash-table
   :restore-hash-table

   :current-seconds
   :open-url
   :generate-temporary-file-name))
