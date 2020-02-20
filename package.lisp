;;;; package.lisp

(defpackage #:mutility
  (:use #:cl
        #:alexandria)
  (:export
   ;; :fn ;; FIX
   ;; :accumulating ;; conflicts with iterate
   :define-obsolete-function-alias

   :concat
   :output
   :split
   :replace-all

   :my-intern
   :un-intern

   :sign
   :wrap

   :length-upto
   :nth-wrap
   :elt-wrap
   :has-any
   :mapcar-with-index
   :flatten-1
   :subseq*
   ;; :repeat ;; conflicts with iterate

   :random-range
   :exponential-random-range
   :random-gauss

   :save-hash-table
   :restore-hash-table

   :current-seconds))