(in-package #:mutility)

(defun swank-ed-in-emacs (file)
  "Edit a file in Emacs via Swank and return true so SBCL doesn't give an error."
  (swank:ed-in-emacs file)
  t)

;; swank doesn't seem to put ed-in-emacs in sbcl's ed-functions
;; FIX: make this work for other implementations
#+sbcl
(unless sb-ext:*ed-functions* (push #'swank-ed-in-emacs sb-ext:*ed-functions*))
