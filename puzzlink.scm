(define-module (puzzlink)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (puzzledb-elm)
  #:use-module (pzpr))

(define-public puzzlink
  (package
    (name "puzzlink")
    (version "current")
    (source #f)
    (build-system trivial-build-system)
    (inputs
     `(("pzpr-puzzlink" ,pzpr-puzzlink)
       ("puzzledb-frontend" ,puzzledb-frontend)))
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (assoc-ref %build-inputs "pzpr-puzzlink")
             %output)
           (copy-recursively
             (assoc-ref %build-inputs "puzzledb-frontend")
             (string-append %output "/db")))))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))
