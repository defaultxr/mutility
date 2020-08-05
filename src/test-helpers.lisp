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

(defun docstrings-with-broken-links (package)
  "Scan the external symbols of PACKAGE for docstring links that point to nonexistent symbols."
  (let (symbols)
    (do-external-symbols (sym package symbols)
      (block next
        (when-let ((linked (flatten (mapcar 'docstring-linked-symbol-names (all-docstrings sym)))))
          (dolist (link linked)
            (when link
              (let ((split (split-string link :char-bag (list #\:))))
                (unless (if (= 1 (length split))
                            (find-symbol (string-upcase (elt split 0)) package)
                            (ignore-errors
                             (find-symbol (string-upcase (elt split 1)) (string-upcase (elt split 0)))))
                  (push sym symbols)
                  (return-from next nil))))))))))

(export '(system-missing-attributes
          undocumented-symbols
          docstrings-with-broken-links))
