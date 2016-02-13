(use-modules (io path)
             (srfi srfi-13)
             (srfi srfi-64))

(define (username)
  (let* ((uid  (getuid))
         (pw   (getpwuid uid))
         (name (passwd:name pw)))
    name))

(test-begin "01-simple")

(test-equal
 (getcwd)
 (current-directory))

(test-assert
 (string-contains (home-directory)
                  (username)))

(test-equal "temporary-directory is '/tmp'"
 "/tmp"
 (temporary-directory))

(test-assert
 (absolute-path? "/tmp"))

(test-assert
 (absolute-path? (home-directory)))

(test-assert
 (absolute-path? (current-directory)))

(test-assert
 (absolute-path? (temporary-directory)))

(test-assert
 (relative-path? "../hoge"))

(test-assert
 (relative-path? "piyo"))

(test-assert
 (relative-path? "./fuga"))

(test-assert
 (not (relative-path? "/tmp")))

(test-assert
 (not (relative-path? (home-directory))))

(test-assert
 (not (relative-path? (current-directory))))

(test-assert
 (not (relative-path? (temporary-directory))))

(test-assert "01-simple.scm is regular file"
             (file-is-regular? "01-simple.scm"))

(test-assert ". is not regular file"
             (not (file-is-regular? ".")))

;; (test-assert "/etc is directory"
;;              (file-is-directory? "/etc"))

;; (test-assert "home-directory is directory"
;;              (file-is-directory? (home-directory)))

;; (test-assert ". is directory"
;;              (file-is-directory? "."))

(test-assert
 (file-execute-access? "/usr/bin/guile"))

(test-assert
 (execution-file? "/usr/bin/guile"))

(test-end "01-simple")
