(use-modules (io path)
             (srfi srfi-64))

(define (username)
  (let* ((uid  (getuid))
         (pw   (getpwuid uid))
         (name (passwd:name pw)))
    name))

(test-begin "02-path-building")

(test-equal "~/var/hoge.txt" (build-path "~/var" "hoge.txt"))
(test-equal "~/var/hoge.txt" (build-path "~" "var" "hoge.txt"))

(let ((excepted (string-append "/home/" (username) "/var/hoge.txt")))

  (test-equal excepted (expand-path "~/var/hoge.txt"))

  (test-equal excepted (build-path "" "home" (username) "var" "hoge.txt")))

(test-end "02-path-building")
