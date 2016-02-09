(use-modules (io path)
             (srfi srfi-64))

(test-begin "01-simple")

(test-equal "~/var/hoge.txt" (build-path "~/var" "hoge.txt"))
(test-equal "~/var/hoge.txt" (build-path "~" "var" "hoge.txt"))

(let ((excepted (string-append "/home/" (getenv "USERNAME") "/var/hoge.txt")))

  (test-equal excepted (expand-path "~/var/hoge.txt"))

  (test-equal excepted (build-path "" "home" (getenv "USERNAME") "var" "hoge.txt")))

(test-end "01-simple")
