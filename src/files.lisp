;;;; files.lisp - Various functions to ease file-related tasks.
;;; NOTES:
;;; - `cl:file-write-date' can be used to get the file modification time.
;;; - `cl:file-author' can be used to get the file's owner.

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

(defconstant +file-extension-separator+ #\.
  "The character separating the file's name from its extension.")

;;; class

(defclass file ()
  ((path :initarg :path :accessor file-path)
   (size :initarg :size :accessor file-size)
   (permissions :initarg :permissions :accessor file-permissions)
   (uid :initarg :uid :accessor file-uid)
   (gid :initarg :gid :accessor file-gid)
   (mtime :initarg :mtime :accessor file-mtime)
   (ctime :initarg :ctime :accessor file-ctime)
   (atime :initarg :atime :accessor file-atime)
   (birth :initarg :birth :accessor file-birth)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t)
    (format stream ":PATH ~S" (file-path file))))

(defun make-file (path)
  (let ((path (uiop:native-namestring path)))
    (make-instance 'file :path path)))

;;; file generics

(defmacro define-file-method (name lambda-list &body body)
  "Define the generic function named NAME, as well as the methods specializing its first parameter on file, string, and pathname. The string and pathname methods effectively forward to the file method."
  (multiple-value-bind (body declarations documentation) (parse-body body :documentation t)
    (declare (ignore declarations)) ; FIX: use declarations?
    `(progn
       (defgeneric ,name ,lambda-list
         ,@(when documentation `((:documentation ,documentation))))
       (defmethod ,name ((,(car lambda-list) file) ,@(cdr lambda-list))
         ,@body)
       (defmethod ,name ((,(car lambda-list) string) ,@(cdr lambda-list))
         (,name (make-file ,(car lambda-list))))
       (defmethod ,name ((,(car lambda-list) pathname) ,@(cdr lambda-list))
         (,name (make-file ,(car lambda-list)))))))

(define-file-method file-path (file)
  "Get the full unabbreviated path to FILE as a string."
  )

(defgeneric file-path (file)
  (:documentation "Get the full unabbreviated path to FILE as a string."))

(defmethod file-path ((file string))
  (uiop:native-namestring
   (uiop:ensure-pathname (if (char= #\~ (char file 0))
                             (concat (namestring (user-homedir-pathname))
                                     (subseq file 2))
                             file)
                         :defaults (uiop:get-pathname-defaults)
                         :dot-dot :up
                         :want-non-wild t
                         :ensure-absolute t
                         :truenamize t)))

(defmethod file-path ((file pathname))
  (file-path (namestring file)))

(defgeneric file-path-no-extension (file)
  (:documentation "Get FILE's path, excluding its extension.

See also: `file-path', `file-name', `file-name-no-extension', `file-extension'"))

(defmethod file-path-no-extension (file)
  (let* ((path (file-path file))
         (ext-pos (position +file-extension-separator+ path :from-end t)))
    (if ext-pos
        (subseq path 0 ext-pos)
        path)))

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
  (let ((name (file-name file)))
    (coerce (butlast (coerce name 'list) (1+ (length (file-extension file)))) 'string)))

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
  (uiop:native-namestring (file-file-p file)))

(defmethod file-file-p ((file pathname))
  (uiop:file-exists-p file))

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

#+(or)
(defmethod file-directory ((file file))
  (file-directory (file-path file)))

(defgeneric file-parent-directory (file)
  (:documentation "Get the containing directory of FILE."))

(defmethod file-parent-directory ((file string))
  (uiop:native-namestring (file-parent-directory (uiop:parse-native-namestring file))))

(defmethod file-parent-directory ((file pathname))
  (make-pathname :directory (butlast (pathname-directory file))))

;;; file "types"

;; audio

(defclass audio-file (file)
  ((artist :initarg :artist :initform nil :documentation "")
   (album-artist :initarg :album-artist :initform nil :documentation "")
   (composer :initarg :composer :initform nil :documentation "")
   (album :initarg :album :initform nil :documentation "")
   (title :initarg :title :initform nil :documentation "")
   (track :initarg :track :initform nil :documentation "")
   (date :initarg :date :initform nil :documentation "")
   (genre :initarg :genre :initform nil :documentation "")
   (time :initarg :time :initform nil :documentation "")
   (bitrate :initarg :bitrate :initform nil :documentation "")
   (sample-rate :initarg :sample-rate :initform nil :documentation "")))

;; image

;; video

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

(defun file-finder (&key max-depth iname name)
  "Find files in the filesystem matching the given predicates."
  ;; FIX: make this user-extensible (maybe it should just forward all unknown keyword args to a function of the same name if one exists (or allow functions to be specified directly))
  ;; FIX: make aliases of keyword arguments for their unix "find" equivalent
  (error "~S is not implemented yet." 'file-finder))

(export '(ensure-directory-trailing-slash

          file-path
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

          locate-dominating-file
          file-finder))
