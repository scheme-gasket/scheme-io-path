(define-module (io path)
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

(define-public (temporary-directory)
  "一時ファイルを作るのに適したディレクトリ名を保持しているパラメータらしいのですが、
ここでは手続きにしています。
それで、どうやら guile の POSIX には ~tmpdir~ が無いようなので環境変数 TMPDIR と TMP がこの順でチェックされ、
フォールバックとして /tmp が返されるということにします。"
  (let ((tmp-dir (getenv "TMPDIR")))
    (if tmp-dir
        (begin
          (set! tmp-dir (getenv "TMP"))
          (if tmp-dir
              tmp-dir
              "/tmp"))
        tmp-dir)))

(define*-public (build-path base-path #:rest components)
  "~component~ of a pathname is added to ~base-path~."
  (string-join (map (lambda (path)
                      (string-trim-both path (string-ref file-name-separator-string 0)))
                    (cons base-path components))
               file-name-separator-string
               'prefix))

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

(define-public (file-is-directory? path)
  ""
  (if (file-exists? path)
      (let ((st (stat path)))
        (eq? 'directory (stat:type st)))
      ;; else
      #f))

(define-public (file-execute-access? path)
  ""
  (access? path X_OK))

(define-public (execution-file? path)
  "~#t~ will be returned if the path of a file can be performed."
  (and (file-is-regular? path)
       (file-execute-access? path)))

(define*-public (build-path base-path #:rest components)
  "The ~component~ of a pathname is added to ~base-path~ ."
  (string-join (map (lambda (path)
                      (string-trim-both path (string-ref file-name-separator-string 0)))
                    (cons base-path components))
               file-name-separator-string
               'prefix))

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
