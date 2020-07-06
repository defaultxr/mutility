;;;; package.lisp

(defpackage #:mutility
  (:use #:cl
        #:alexandria)
  (:export
   :a
   :fn
   :dolist*
   ;; :accumulating ;; conflicts with iterate
   :define-obsolete-function-alias

   :keys

   :friendly-symbol

   :concat
   :output
   :split-string
   :replace-all
   :string-boolean
   :friendly-ratio-string
   :friendly-duration-string

   :my-intern
   :un-intern

   :wrap
   :floor-by
   :round-by
   :round-by-direction

   :length-upto
   :nth-wrap
   :elt-wrap
   :find-any
   :mapcar-with-index
   :flatten-1
   :subseq*
   ;; :repeat ;; conflicts with iterate
   :split-sequence
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
