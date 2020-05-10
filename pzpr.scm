(define-module (pzpr)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial))

(define-public pzprjs
  (package
    (name "pzprjs")
    (version "updated")
    (source "/home/rob/pzprjs")
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (string-append (assoc-ref %build-inputs "source") "/dist")
             %output)
           #t)))
    (home-page "fake home page to cause rebuild")
    (synopsis #f)
    (license #f)
    (description #f)))
