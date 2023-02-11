;;;; emacs-extensions.lisp - emacs-related extension functionality.

(in-package #:mutility)

(defun emacs-find-file (file)
  "Open a file in Emacs."
  (let ((file (typecase file
                (string file)
                (pathname (namestring file)))))
    (uiop:launch-program `("emacsclient" ,file))))

(defun emacs-ed (file)
  "Call `emacs-find-file' to open FILE in Emacs, and then return true so SBCL doesn't give an error."
  (emacs-find-file file)
  t)

;; swank doesn't seem to put ed-in-emacs in sbcl's ed-functions
;; FIX: make this work for other implementations
#+sbcl
(unless sb-ext:*ed-functions*
  (push #'emacs-ed sb-ext:*ed-functions*))
