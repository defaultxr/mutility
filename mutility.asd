;;;; mutility.asd

(asdf:defsystem #:mutility
  :description "modula's utilities."
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.5"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "mutility")))
