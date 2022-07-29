;;;; test-helpers - functions that may be helpful for writing tests

(in-package #:mutility)

;;; docstrings

(defun docstring-linked-symbol-names (string)
  "Get all linked symbol names in STRING."
  (when-let* ((length (length string))
              (start (position #\` string))
              (cur (1+ start)))
    (loop :while (and (> length cur)
                      (not (char= #\' (char string cur)))) ; FIX: check for other invalid chars here like backticks and spaces
          :do (incf cur))
    (cons (subseq string (1+ start) cur)
          (docstring-linked-symbol-names (subseq string cur)))))

(defun symbol-all-docstrings (symbol)
  "Get a list of all docstrings for SYMBOL."
  (let (res)
    (dolist (type (list 'variable 'function 'type 'structure 'setf 'method-combination) res)
      (when-let ((doc (documentation symbol type)))
        (push doc res)))))

(defun docstring-broken-links (docstring &key (package *package*) scan-external-packages)
  "Get a list of broken links in DOCSTRING. PACKAGE is the package to look for non-package-prefixed symbols in.

SCAN-EXTERNAL-PACKAGES accepts one of three values:

- nil - Ignore package-prefixed symbol names.
- t - Check them against symbols in the current Lisp image.
- :autoload - Similar to t, but when a symbol prefixed with a package that doesn't exist in the Lisp image is found, attempt to load the system that might contain it by `ql:quickload'ing the package name, then checking again."
  (let (missing)
    (when-let ((linked (docstring-linked-symbol-names docstring)))
      (dolist (link linked missing)
        ;; (when link) ; not sure why i had this here?
        (let* ((split (string-split link :char-bag (list #\:)))
               (link-name (lastcar split))
               (link-has-package-name (not (length= 1 split)))
               (link-package-name (if link-has-package-name
                                      (string-upcase (car split))
                                      package))
               (link-package (or (find-package link-package-name)
                                 (when (and (eql :autoload scan-external-packages)
                                            (find-package 'quicklisp))
                                   (format *debug-io* "~&Attempting to auto-quickload ~S...~%" link-package-name)
                                   (ignore-errors
                                    (uiop:symbol-call 'quicklisp "QUICKLOAD" link-package-name))
                                   (find-package link-package-name)))))
          (when (or (not link-has-package-name)
                    (and link-has-package-name
                         scan-external-packages))
            (unless (ignore-errors (find-symbol (string-upcase link-name) link-package))
              (push link missing))))))))

;;; systems

(defun system-missing-attributes (system)
  "Get a list of attributes missing from SYSTEM that should probably be added. The following attributes are checked:

- name
- description
- author
- license
- version
- homepage
- bug-tracker
- mailto
- source-control"
  (let ((system (asdf:find-system system)))
    (remove-if (fn (funcall _ system))
               (list 'asdf:primary-system-name
                     'asdf:system-description
                     'asdf:system-author
                     'asdf:system-license
                     'asdf:component-version
                     'asdf:system-homepage
                     'asdf:system-bug-tracker
                     'asdf:system-mailto
                     'asdf:system-source-control))))

;;; packages

(define-constant +documentation-types+ (list 'function 'variable 'method-combination 'compiler-macro 'setf 'structure 'type)
  :test #'equal
  :documentation "The set of standard Common Lisp documentation types.")

(defun package-undocumented-symbols (package)
  "Get a list of all the undocumented external symbols in PACKAGE."
  (let (symbols)
    (do-external-symbols (sym package symbols)
      (unless (position-if (curry #'documentation sym) +documentation-types+)
        (push sym symbols)))))

(defun package-symbol-conflicts (package &rest other-packages)
  "Get a list of symbols exported by any of OTHER-PACKAGES that conflict with the symbols exported by PACKAGE."
  (let (package-exported-symbols
        conflicts)
    (do-external-symbols (symbol package)
      (push symbol package-exported-symbols))
    (dolist (other-package other-packages conflicts)
      (do-external-symbols (symbol other-package)
        (when (find symbol package-exported-symbols :test #'string=)
          (push symbol conflicts))))))

(defun package-docstrings-with-broken-links (package &key scan-external-packages)
  "Scan the external symbols of PACKAGE for docstring links that point to nonexistent symbols. If SCAN-EXTERNAL-PACKAGES is true, any links in docstrings that point to symbols from other packages will cause that package to be loaded to check if the link is broken."
  (let (results)
    (do-external-symbols (symbol package results)
      (when-let ((missing (mapcan (rcurry #'docstring-broken-links :package package
                                                                   :scan-external-packages scan-external-packages)
                                  (symbol-all-docstrings symbol))))
        (push (list* symbol missing) results)))))

(uiop:with-deprecation (:style-warning)
  (defun undocumented-symbols (package)
    "Deprecated alias for `package-undocumented-symbols'."
    (package-undocumented-symbols package))

  (defun all-docstrings (symbol)
    "Deprecated alias for `symbol-all-docstrings'."
    (symbol-all-docstrings symbol)))

(export '(docstring-linked-symbol-names
          symbol-all-docstrings
          docstring-broken-links

          system-missing-attributes

          package-undocumented-symbols
          package-symbol-conflicts
          package-docstrings-with-broken-links

          ;; deprecated
          undocumented-symbols
          all-docstrings))
