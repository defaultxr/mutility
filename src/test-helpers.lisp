;;;; test-helpers - functions that may be helpful for writing tests

(in-package #:mutility)

;;; org-mode

(defun org-header-line-p (line)
  "True if LINE is an org header line (i.e. starts with at least one asterisk and a space). Returns two values: the header text, and the header level (number of asterisks).

Examples:

;; (org-header-line-p \"* cool header\")
;; ;=> \"cool header\"
;; ;=> 1

;; (org-header-line-p \"*** bar baz\")
;; ;=> \"bar baz\"
;; ;=> 3

See also: `org-list-line-p', `stream-extract-org-headers', `file-extract-org-headers'"
  (let ((split (string-split line :char-bag (list #\space) :count 2)))
    (and (length= 2 split)
         (every (lambda (char)
                  (char= #\* char))
                (first split))
         (values (second split)
                 (length (first split))))))

(defun org-list-line-p (line)
  "True if LINE is an org list line (i.e. starts with zero or more spaces, a dash, and a space). Returns two values: the text, and the list level (one plus the number of leading spaces).

Examples:

;; (org-list-line-p \"- foo\")
;; ;=> \"foo\"
;; ;=> 1

;; (org-list-line-p \"  - bar baz\")
;; ;=> \"bar baz\"
;; ;=> 3

See also: `org-header-line-p', `stream-extract-org-lists', `file-extract-org-lists'"
  (let ((num-spaces (position-if-not (lambda (char) (char= #\space char)) line)))
    (and num-spaces
         (string= "- " (subseq line num-spaces (+ 2 num-spaces)))
         (values (subseq line (+ 2 num-spaces))
                 (1+ num-spaces)))))

(defun stream-extract-org-headers (stream)
  "Get a list of the text of all header lines in STREAM.

See also: `file-extract-org-headers', `org-header-line-p'"
  (loop :for line := (read-line stream nil nil)
        :for header := (when line (org-header-line-p line))
        :if (and line header)
          :collect header
        :unless line
          :do (loop-finish)))

(defun file-extract-org-headers (file)
  "Get all the Org-Mode-formatted headers in FILE.

See also: `stream-extract-org-headers', `org-header-line-p'"
  (with-open-file (stream file :if-does-not-exist :error)
    (stream-extract-org-headers stream)))

(defun stream-extract-org-header (stream header &key level)
  "Get the org header matching HEADER in STREAM. LEVEL specifies how many asterisks the headline should have, or nil to match any headline. Returns the contents of the header as a second value.

See also: `file-extract-org-header', `org-header-line-p'"
  (destructuring-bind (header-text . contents)
      (loop :with found-level := nil
            :for line := (read-line stream nil nil)
            :for (header-text header-level) := (when line
                                                 (multiple-value-list (org-header-line-p line)))
            :if (or (null line)
                    (and found-level
                         header-level
                         (>= found-level header-level)))
              :do (loop-finish)
            :if found-level
              :collect line
            :if (and (not found-level)
                     header-text
                     (search header header-text :test #'char=)
                     (if level
                         (eql level header-level)
                         t))
              :collect (progn (setf found-level header-level)
                              header-text))
    (values header-text (string-join* contents #.(string #\newline)))))

(defun file-extract-org-header (file header &key level)
  "Get the org header matching HEADER in STREAM. If GET-CONTENTS is true, also return the contents of the header (i.e. the text between this header and the next one of the same level) as a second value.

See also: `stream-extract-org-header', `org-header-line-p'"
  (with-open-file (stream file :if-does-not-exist :error)
    (stream-extract-org-header stream header :level level)))

(defun stream-extract-org-lists (stream)
  "Get a list of the text of all header lines in STREAM.

See also: `file-extract-org-lists', `org-list-line-p'"
  (loop :for line := (read-line stream nil nil)
        :for list-item := (when line (org-list-line-p line))
        :if (and line list-item)
          :collect list-item
        :unless line
          :do (loop-finish)))

(defun file-extract-org-lists (file)
  "Get all the Org-Mode-formatted headers in FILE.

See also: `stream-extract-org-lists', `org-list-line-p'"
  (with-open-file (stream file :if-does-not-exist :error)
    (stream-extract-org-lists stream)))

(defun stream-extract-org-links (stream)
  "Get a list of substrings in STREAM enclosed between OPEN-CHAR and CLOSE-CHAR. Note that substrings spanning multiple lines are not 

See also: `file-extract-org-links', `balanced-subsequences'"
  (loop :for line := (read-line stream nil nil)
        :if line
          :append (balanced-subsequences line :open #\[ :close #\] :test #'char=)
        :else
          :do (loop-finish)))

(defun file-extract-org-links (file)
  "Get all the Org-Mode-formatted links in FILE.

See also: `stream-extract-org-links'"
  (with-open-file (stream file :if-does-not-exist :error)
    (stream-extract-org-links stream)))

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
    (symbol-all-docstrings symbol))

  (defun docstrings-with-broken-links (package &key scan-external-packages)
    "Deprecated alias for `package-docstrings-with-broken-links'."
    (package-docstrings-with-broken-links package :scan-external-packages scan-external-packages)))

(export '(org-header-line-p
          org-list-line-p
          stream-extract-org-headers
          file-extract-org-headers
          stream-extract-org-header
          file-extract-org-header
          stream-extract-org-lists
          file-extract-org-lists
          stream-extract-org-links
          file-extract-org-links

          docstring-linked-symbol-names
          symbol-all-docstrings
          docstring-broken-links

          system-missing-attributes

          package-undocumented-symbols
          package-symbol-conflicts
          package-docstrings-with-broken-links

          ;; deprecated
          undocumented-symbols
          all-docstrings
          docstrings-with-broken-links))
