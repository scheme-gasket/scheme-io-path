(add-to-load-path "../lib")

(use-modules (io path)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "03-directory-listing")

(test-equal
 "gcc"
 (find (lambda (progname)
        (equal? "gcc" progname))
      (directory-list "/usr/bin")))

(test-equal
 "/sbin/guile"
 (find (lambda (progname)
        (equal? "/sbin/guile" progname))
      (directory-list "/sbin" #:add-path? #t)))

(test-end "03-directory-listing")
