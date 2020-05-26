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

   :my-intern
   :un-intern

   :wrap
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

   :random-coin
   :random-range
   :exponential-random-range
   :random-gauss

   :save-hash-table
   :restore-hash-table

   :current-seconds
   :open-url
   :generate-temporary-file-name))
