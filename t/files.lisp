;;;; t/files.lisp - tests for mutility file-related functionality.

(in-package #:mutility/tests)

(in-suite mutility-tests)

(test ensure-directory-trailing-slash
  "Test the `ensure-directory-trailing-slash' function"
  )

(test make-file
  "Test the `make-file' function"
  )

(test file-path
  "Test the `file-path' function"
  (is (string= "/tmp/???" (file-path "/tmp/???")))
  (is (string= "/tmp/???" (file-path #P"/tmp/???")))
  (is (string= (namestring (user-homedir-pathname)) (file-path "~/"))))

(test file-name
  "Test the `file-name' function"
  (is (string= "/" (file-name "/")))
  (is (string= "home" (file-name "/home")))
  (is (string= "home" (file-name "/home/")))
  (is (string= "foo.bar" (file-name "/home/user/foo.bar")))
  (is (string= "*scratch*" (file-name "/tmp/*scratch*")))
  (is (string= "???" (file-name "/tmp/???")))
  (is (string= "???" (file-name #P"/tmp/???"))))

(test file-name-no-extension
  "Test the `file-name-no-extension' function"
  (is (string= "foo" (file-name-no-extension "foo.bar")))
  (is (string= "foo" (file-name-no-extension "/tmp/foo.bar"))))

(test file-extension
  "Test the `file-extension' function"
  (is (string= "blah" (file-extension "foo.blah")))
  (is (string= "BlAh" (file-extension "foo.BlAh"))))

(test file-type
  "Test the `file-type' function"
  (is (eql :audio (file-type "foo.wav")))
  (is (eql :video (file-type "foo.mp4")))
  (is (eql :image (file-type "foo.png")))
  (is (eql :archive (file-type "foo.zip"))))

(test file-exists-p
  "Test the `file-exists-p' function"
  (uiop:with-temporary-file (:pathname filename)
    (is (file-exists-p filename))
    (is (file-exists-p (pathname filename)))))

(test file-file-p
  "Test the `file-file-p' function"
  (uiop:with-temporary-file (:pathname temp-file)
    (is (file-file-p temp-file))))

(test file-directory-p
  "Test the `file-directory-p' function"
  #+unix (is-true (file-directory-p "/tmp"))
  (is (file-directory-p (uiop:temporary-directory))))

(test file-directory
  "Test the `file-directory' function"
  (let ((temp-dir (uiop:temporary-directory)))
    (is (string= (namestring temp-dir)
                 (namestring (file-directory temp-dir))))))

(test file-parent-directory
  "Test the `file-parent-directory' function"
  (let ((temp-dir (concat (namestring (uiop:temporary-directory)) "foo/bar")))
    (is (string= (namestring (uiop:temporary-directory)) (file-parent-directory temp-dir))))
  (is (string= "/" (file-parent-directory "/")))
  ;; (is (string= "C:\\" (file-parent-directory "C:\\")))
  )

(test file-parent-directories
  "Test the `file-parent-directories' function"
  ;; FIX
  )

(test file-path-in-directory-p
  "Test the `file-path-in-directory-p' function"
  (is (file-path-in-directory-p "/foo" "/"))
  (is (file-path-in-directory-p "/foo/bar" "/foo"))
  (is (file-path-in-directory-p "/foo/bar" "/foo/"))
  (is (file-path-in-directory-p "/foo/bar/baz" "/"))
  (is (file-path-in-directory-p "/foo/bar/baz" "/foo"))
  (is (file-path-in-directory-p "/foo/bar/baz" "/foo/bar"))
  (is (file-path-in-directory-p "/foo/bar/baz" "/foo/bar/"))
  (is-false (file-path-in-directory-p "/foof" "/foo/bar/"))
  (is-false (file-path-in-directory-p "/foo/bab" "/foo/bar/")))

(test file-relativize
  "Test the `file-relativize' function"
  ;; FIX
  )

(test locate-dominating-file
  "Test the `locate-dominating-file' function"
  ;; FIX
  )

(test file-finder
  "Test the `file-finder' function"
  ;; FIX
  )

(test directory-contents
  "Test the `directory-contents' function"
  ;; FIX
  )
