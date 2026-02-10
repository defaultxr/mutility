;;;; files.lisp - Various functions to ease file-related tasks.
;;; NOTES:
;; - `cl:file-write-date' can be used to get the file modification time.
;; - `cl:file-author' can be used to get the file's owner.

(in-package #:mutility)

;;; utility

(defgeneric ensure-directory-trailing-slash (path)
  (:documentation "Ensure that PATH ends in a slash if it is a directory."))

(defmethod ensure-directory-trailing-slash ((path string))
  (let ((slash (uiop:directory-separator-for-host)))
    (if (file-directory-p path)
        (if (char= slash (last-elt path))
            path
            (concat path slash))
        path)))

(defmethod ensure-directory-trailing-slash ((path pathname))
  (uiop:parse-native-namestring (ensure-directory-trailing-slash (uiop:native-namestring path))))

(define-constant +file-extension-separator+ #\.
  :documentation "The character separating the file's name from its extension."
  :test #'char=)

;;; file generics

(defgeneric file-path (file)
  (:documentation "Get the full unabbreviated path to FILE as a string.

See also: `uiop:absolute-pathname-p', `uiop:relative-pathname-p'"))

(defmethod file-path ((file string))
  (uiop:native-namestring
   (uiop:ensure-pathname (if (char= #\~ (char file 0))
                             (concat (namestring (user-homedir-pathname))
                                     (subseq file 1))
                             file)
                         :defaults (uiop:get-pathname-defaults)
                         :dot-dot :up
                         :want-non-wild t
                         :ensure-absolute t
                         :truenamize t)))

(defmethod file-path ((file pathname))
  (file-path (namestring file)))

(defun file-no-extension (string)
  "Get STRING, without the file extension."
  (if-let ((ext-pos (position +file-extension-separator+ string :from-end t)))
    (subseq string 0 ext-pos)
    string))

(defgeneric file-path-no-extension (file)
  (:documentation "Get FILE's path, excluding its extension.

See also: `file-path', `file-name', `file-name-no-extension', `file-extension'"))

(defmethod file-path-no-extension (file)
  (file-no-extension (file-path file)))

(defgeneric file-name (file)
  (:documentation "Get FILE's name, excluding the directory containing it.

See also: `file-directory', `file-name-no-extension'"))

(defmethod file-name (file)
  (let* ((pathname (file-path file))
         (sep (uiop:directory-separator-for-host))
         (root (namestring (uiop:pathname-root pathname))))
    (if (string= root pathname)
        pathname
        (let ((trimmed (string-right-trim (list sep) pathname)))
          (if-let ((last-sep (position sep trimmed :from-end t :test #'char=)))
            (subseq trimmed (1+ last-sep))
            pathname)))))

(defgeneric file-name-no-extension (file)
  (:documentation "Get FILE's name, excluding the directory containing it, and excluding its extension.

See also: `file-name'"))

(defmethod file-name-no-extension (file)
  (file-no-extension (file-name file)))

(defgeneric file-extension (file)
  (:documentation "The file's extension, i.e. the string following the last period in the filename.

See also: `file-type'"))

(defmethod file-extension ((file string))
  (when-let* ((pos (position +file-extension-separator+ file :from-end t)))
    (subseq file (1+ pos))))

(defmethod file-extension ((file pathname))
  (file-extension (namestring file)))

(defvar *file-extensions-type-map*
  '((:audio :aac :aif :aiff :flac :m4a :mp3 :ogg :opus :wav :wma)
    (:image :bmp :gif :jpeg :jpg :png :webp)
    (:video :avi :flv :m2t :m4v :mkv :mov :mp4 :ogv :webm :wmv)
    (:archive :7z :bz2 :gz :xz :rar :tar :zip))
  "Alist mapping generalized file types to extensions that comprise that type.

See also: `file-type'")

(defgeneric file-type (file)
  (:documentation "Get a generalized \"file type\" for FILE, i.e. image, video, etc."))

(defmethod file-type (file) ; FIX
  (let ((ext (upcase-intern (file-extension file) :keyword)))
    (car (find-if (fn (position ext (cdr _))) *file-extensions-type-map*))))

(defgeneric file-exists-p (file)
  (:documentation "True if FILE names a file or directory that exists."))

(defmethod file-exists-p (file)
  (probe-file (uiop:ensure-pathname file :want-non-wild t)))

(defgeneric file-file-p (file)
  (:documentation "True if FILE is a file (i.e. it exists and is not a directory)."))

(defmethod file-file-p ((file string))
  (uiop:file-exists-p file))

(defmethod file-file-p ((file pathname))
  (file-file-p (uiop:native-namestring file)))

(defgeneric file-directory-p (file)
  (:documentation "True if FILE refers to a directory that exists."))

(defmethod file-directory-p (file)
  (uiop:directory-exists-p file))

(defgeneric file-directory (file)
  (:documentation "Get FILE's path if it is a directory, or the directory containing it if it is a file."))

(defmethod file-directory ((file string))
  (ensure-directory-trailing-slash
   (if (file-directory-p file)
       file
       (uiop:native-namestring (make-pathname :directory (pathname-directory file))))))

(defmethod file-directory ((file pathname))
  (ensure-directory-trailing-slash
   (if (file-directory-p file)
       file
       (make-pathname :directory (pathname-directory file)))))

(defgeneric file-parent-directory (file)
  (:documentation "Get the containing directory of FILE."))

(defmethod file-parent-directory ((file string))
  (uiop:native-namestring (file-parent-directory (uiop:parse-native-namestring file))))

(defmethod file-parent-directory ((file pathname))
  (uiop:pathname-parent-directory-pathname file))

(defun file-parent-directories (file)
  "Get a list of the containing and parent directories of FILE.

See also: `locate-dominating-file'"
  (labels ((parents (dir)
             (list* dir (let ((parent (file-parent-directory dir)))
                          (unless (string= dir parent)
                            (parents parent))))))
    (list* (file-directory file)
           (parents (file-parent-directory file)))))

(defun file-path-in-directory-p (file directory) ; FIX: &optional (allow-subdirectories t)
  "True if FILE's name suggests it is inside DIRECTORY." ; FIX: With ALLOW-SUBDIRECTORIES, also true if FILE is in a subdirectory of DIRECTORY.
  (let* ((file (file-path file))
         (directory (file-path directory))
         (flen (length file))
         (dlen (length directory)))
    (when (>= flen dlen)
      (string= file directory :end1 dlen :end2 dlen))))

(defun file-relativize (file &key (relative-to ) (if-not-in t))
  "Get the path to FILE, relative to RELATIVE-TO. IF-NOT-IN determines how to proceed if FILE is not inside RELATIVE-TO:

- If t, relativize anyway.
- If nil or :absolute, return the full path to FILE.
- If :error, then raise an error.

See also: `file-path-in-directory-p'"
  (if (or (file-path-in-directory-p file relative-to)
          (eql if-not-in t))
      (namestring (uiop:enough-pathname file relative-to))
      (ecase if-not-in
        (:error (error "File ~S is not inside ~S" file relative-to))
        ((:absolute nil) (file-path file)))))

;;; traversal

(defun locate-dominating-file (directory name)
  "Check for a file named NAME in DIRECTORY. If no such file is found, search again in the parent directory. Continue traversing up the directory tree until either finding the file, or hitting the filesystem root. Returns the full path to the file if found, or nil if no such file was found.

This is equivalent to the Emacs function of the same name."
  (labels ((try-next (directory)
             (let ((check (uiop:merge-pathnames* name directory)))
               (if (file-exists-p check)
                   check
                   (let ((next (file-parent-directory directory)))
                     (if (uiop:pathname-equal directory next)
                         nil
                         (try-next next)))))))
    (when-let* ((full-dir (ensure-directory-trailing-slash (file-path (uiop:parse-native-namestring directory))))
                (res (try-next full-dir)))
      (if (stringp directory)
          (uiop:native-namestring res)
          res))))

(defgeneric directory-contents (directory)
  (:documentation "Get a list of the items in DIRECTORY."))

(defmethod directory-contents ((directory string))
  (mapcar #'file-path (uiop:directory* (concat (string-right-trim (list #\/) directory) "/*.*"))))

(defmethod directory-contents ((directory pathname))
  (directory-contents (namestring directory)))

(export '(ensure-directory-trailing-slash

          file-path
          file-no-extension
          file-path-no-extension
          file-name
          file-name-no-extension

          file-extension
          file-type

          file-exists-p
          file-file-p
          file-directory-p

          file-directory
          file-parent-directory
          file-parent-directories
          file-path-in-directory-p
          file-relativize

          locate-dominating-file

          directory-contents))
