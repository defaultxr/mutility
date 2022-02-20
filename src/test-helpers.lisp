(in-package #:mutility)

;;; test-helpers - functions that may be helpful for writing tests

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

(defun undocumented-symbols (package)
  "Get a list of all the undocumented external symbols in PACKAGE."
  (let (symbols)
    (do-external-symbols (sym package symbols)
      (unless (position-if (lambda (type)
                             (documentation sym type))
                           (list 'function 'variable 'method-combination 'compiler-macro 'setf 'structure 'type))
        (push sym symbols)))))

(defun docstring-linked-symbol-names (string)
  "Get all linked symbol names in STRING."
  (when-let* ((length (length string))
              (start (position #\` string))
              (cur (1+ start)))
    (loop :while (and (> length cur)
                      (not (char= #\' (char string cur)))) ;; FIX: check for other invalid chars here like backticks and spaces
          :do (incf cur))
    (cons (subseq string (1+ start) cur)
          (docstring-linked-symbol-names (subseq string cur)))))

(defun all-docstrings (symbol)
  "Get a list of all docstrings for SYMBOL."
  (let (res)
    (dolist (type (list 'variable 'function 'type 'structure 'setf 'method-combination) res)
      (when-let ((doc (documentation symbol type)))
        (push doc res)))))

(defun docstrings-with-broken-links (package &optional scan-external-packages)
  "Scan the external symbols of PACKAGE for docstring links that point to nonexistent symbols. If SCAN-EXTERNAL-PACKAGES is true, any links in docstrings that point to symbols from other packages will cause that package to be loaded to check if the link is broken."
  (let (symbols)
    (do-external-symbols (symbol package symbols)
      (let (missing
            (linked (flatten (mapcar 'docstring-linked-symbol-names (all-docstrings symbol)))))
        (when linked
          (dolist (link linked)
            (when link
              (let* ((split (string-split link :char-bag (list #\:)))
                     (link-name (lastcar split))
                     (link-package-name (if (= 1 (length split))
                                            package
                                            (string-upcase (car split))))
                     (link-package (let ((pack (find-package link-package-name)))
                                     (if pack
                                         pack
                                         (when scan-external-packages
                                           (when (find-package 'quicklisp)
                                             (funcall (find-symbol "QUICKLOAD" 'quicklisp) link-package-name))
                                           (find-package link-package-name))))))
                (when (or link-package scan-external-packages)
                  (unless (ignore-errors (find-symbol (string-upcase link-name) link-package))
                    (push link missing))))))
          (when missing
            (push (list symbol missing) symbols)))))))

(export '(system-missing-attributes
          undocumented-symbols
          docstrings-with-broken-links))
