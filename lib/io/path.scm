(define-module (io path)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 optargs))


(define*-public (current-directory #:optional (new-director #f))
  "A current directory is returned when called without an argument.
When character string~new-directory~ is given, the current directory of a process is changed into ~new-directory~.
It becomes an error when change is not completed. "
  (getcwd))

(define*-public (home-directory #:optional user)
  "The home directory of user~user~ given by the user ID of the name or the integer is returned.
A current user is used when ~user~ is omitted.
~#f~ is returned, when the given user is not found or a home directory is not able to be determined."
  (let ((home-dir (getenv "HOME")))
    (if home-dir
        home-dir
        ;; else
        (passwd:dir (getpwuid (getuid))))))

(define-public temporary-directory
  (make-parameter (let ((tmp-dir (find string? (list (getenv "TMPDIR")
                                                     (getenv "TMP")
                                                     "/tmp"))))
                    tmp-dir)))

(define-public absolute-path? absolute-file-name?)

(define-public (relative-path? path)
  "~#t~ will be returned if ~path~ is a relative path."
  (or (string-prefix? "." path)
      (string-prefix? ".." path)
      (not (string-prefix? "/" path))))

(define-public (file-is-regular? path)
  "~#t~ will be returned if the path of a file is a regular entry."
  (if (file-exists? path)
      (let ((st (stat path)))
        (eq? 'regular (stat:type st)))
      ;; else
      #f))

;; (define-public (file-is-directory? path)
;;   ""
;;   (if (file-exists? path)
;;       (let ((st (stat path)))
;;         (eq? 'directory (stat:type st)))
;;       ;; else
;;       #f))

(define-public (file-execute-access? path)
  ""
  (access? path X_OK))

(define-public (execution-file? path)
  "~#t~ will be returned if the path of a file can be performed."
  (and (file-is-regular? path)
       (file-execute-access? path)))

(define*-public (build-path base-path #:rest components)
  "~component~ of a pathname is added to ~base-path~."
  (string-join (map (lambda (path)
                      (string-trim-both path (string-ref file-name-separator-string 0)))
                    (cons base-path components))
               file-name-separator-string
               'infix))

(define-public (expand-path path)
  "If ~path~ includes the tilde display, what developed it will be returned.
Otherwise, ~path~ itself is returned.
~path~ exists and whether it can access or not does not check this procedure. "
  (if (string-prefix? "~" path)
      (let ((home-dir (home-directory)))
        (string-append home-dir (string-drop path 1)))
      ;; else
      path))

(define-public (decompose-path path)
  "Path name ~path~ directory part of the file name without the extension, and returns three of the value of the extension.
The last value becomes ~#f~ when a pathname does not have an extension.
When the pathname has finished with the directory separator, it is the 2nd and 3. The value of eye watch (1 based) is set to ~#f~. "
  (let* ((dir-path   (dirname path))
         (extention  (path-extension path))
         (base-filename (if extention
                            (basename path (string-append "." extention))
                            ;; else
                            #f)))
    (values dir-path
            base-filename
            extention)))

(define-public (path-extension path)
  "The extension of ~path~ is returned.
~#f~ is returned when ~path~ does not have an extension."
  (let ((found-index (string-index-right path #\.)))
    (if found-index
        (string-copy path (+ found-index 1))
        ;; else
        #f)))

(define (gen-append-pathname pathname)
  (lambda (entry)
    (if (string-suffix?  "/" pathname)
        (string-append pathname entry)
        ;; else
        (string-append pathname "/" entry))))

(define* (in-dir pathname #:optional (proc #f) (filter (lambda (e) #t)) (filter-add-path? #f))
  (define append-pathname (gen-append-pathname pathname))
  (when filter-add-path?
        (let ((old-filter filter))
          (set! filter (lambda (entry)
                         (old-filter (append-pathname entry))))))
  (let ((dir      (opendir pathname)))
    (let recur ((entry (readdir dir)))
      (cond ((eof-object? entry)    #f)
            (else                   (when proc
                                          (when (filter entry)
                                                (proc entry)))
                                    (recur (readdir dir)))))
    (closedir dir)))

(define (current-or-parent? path)
  (or (string=? path ".")
      (string=? path "..")))

(define*-public (directory-list pathname #:key (children? #f) (add-path? #f) (filter (lambda (e) #t)) (filter-add-path? #f))
  ""
  (define append-pathname (gen-append-pathname pathname))
  (let ((entries '()))
    (define in-dir-proc (cond ((and children?
                                    add-path?)         (lambda (entry)
                                                         (if (current-or-parent? entry)
                                                             #f
                                                             ;; else
                                                             (set! entries (cons (append-pathname entry) entries)))))
                              (children?               (lambda (entry)
                                                         (if (current-or-parent? entry)
                                                             #f
                                                             ;; else
                                                             (set! entries (cons entry entries)))))
                              (add-path?               (lambda (entry)
                                                         (set! entries (cons (append-pathname entry) entries))))
                              (else                    (lambda (entry)
                                                         (set! entries (cons entry entries))))))
    (in-dir pathname in-dir-proc filter filter-add-path?)
    (sort entries string<?)))
