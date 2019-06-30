(define-module (build-elm)
  #:use-module (json parser)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw) ; scandir
  #:use-module (guix build utils)
  #:export (elm-package-and-version elm-unpack))

(define* (elm-package-and-version pkg)
  "Read the package name and version from an elm package."
  (catch 'system-error
    (lambda _
      (let* ((filename (string-append pkg "/elm.json"))
             (json (json->scm (open-input-file filename))))
        `(,(hash-ref json "name") . ,(hash-ref json "version"))))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define* (elm-unpack pkg dest)
  "Unpack an elm source archive. DEST is relative."
  (mkdir "elm-unpack")
  (with-directory-excursion "elm-unpack"
    (invoke "tar" "xzf" pkg)
    (let* ((entries (scandir "." (lambda (f) (> (string-length f) 2))))
           (entry (car entries))
           (n-v (elm-package-and-version entry))
           (n (car n-v))
           (v (cdr n-v)))
      (mkdir-p (string-append "../" dest "/" n))
      (rename-file entry (string-append "../" dest "/" n "/" v))
      (format #t "unpacked ~a/~a~%" n v)))

           
;    (match (scandir "." (lambda (f) (> (string-length f) 2)))
;      ((pkgdir)
;       (match (elm-package-and-version pkgdir)
;         ((name . version)
;          ((format #t " ~a ~a~%" name version)
;           (mkdir-p (string-append dest "/" name))
;           (rename-file pkgdir (string-append dest "/" name "/" version))
;          )
;         )
;       )
;      )
;    )
;    (format #t "don't get here~%")
;  )
  (delete-file-recursively "elm-unpack")
  #t)

