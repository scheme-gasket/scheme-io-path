(add-to-load-path "../lib")

(use-modules (srfi srfi-1)
             (io path))

(for-each (lambda (path)
            (display path)
            (newline))
          (map (lambda (dirpath)
                 (directory-list dirpath
                                 #:add-path? #t
                                 #:filter (lambda (entry)
                                            (and (execution-file? entry)
                                                 (equal? (basename "gcc"))))
                                 #:filter-add-path? #t))
               (parse-path (getenv "PATH"))))

