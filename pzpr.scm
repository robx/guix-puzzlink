(define-module (pzpr)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial))

(define-public pzprjs
  (package
    (name "pzprjs")
    (version "git-dist")
    (source
      (origin
        (method git-fetch)
        (file-name (git-file-name name version))
        (uri
          (git-reference
            (url "https://github.com/robx/pzprjs.git")
            (commit "3d27b536d362aed24b7f0e2261322c0ccd46f6cb")))
        (sha256
          (base32
            "12n97fybj5v20dwlqh4dl0hzf89r5bzlhrx82kwpsh44pagy2i4h"))))
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (string-append (assoc-ref %build-inputs "source") "/dist")
             (string-append %output "/js"))
           #t)))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))

(define-public pzprjs-local
  (package
    (name "pzprjs-local")
    (version "local")
    (source "/home/rob/pzprjs")
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (string-append (assoc-ref %build-inputs "source") "/dist")
             (string-append %output "/js"))
           #t)))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))
