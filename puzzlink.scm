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
    (version "none-really")
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

(define-public pzpr-puzzlink
  (package
    (name "pzpr-puzzlink")
    (version "git-dist")
    (source
      (origin
        (method git-fetch)
        (file-name (git-file-name name version))
        (uri
          (git-reference
            (url "https://github.com/robx/pzpr-puzzlink.git")
            (commit "dist")))
        (sha256
          (base32
            "1bpmg71q3hwlkfpf7k167iw9h3jpxs0l0nn2vhfc1kaa7vi7idch"))))
    (build-system trivial-build-system)
    (inputs `(("pzprjs" ,pzprjs)))
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (string-append (assoc-ref %build-inputs "source") "/dist")
             %output)
           (copy-recursively
             (string-append (assoc-ref %build-inputs "pzprjs") "/js")
             (string-append %output "/js"))
           #t)))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))

(define-public puzzlink-test
  (package
    (name "puzzlink-test")
    (version "none-really")
    (source #f)
    (build-system trivial-build-system)
    (inputs
     `(("pzpr-puzzlink" ,pzpr-puzzlink-local)))
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (assoc-ref %build-inputs "pzpr-puzzlink")
             %output))))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))

(define-public pzpr-puzzlink-local
  (package
    (name "pzpr-puzzlink-local")
    (version "git-dist")
    (source
      (origin
        (method git-fetch)
        (file-name (git-file-name name version))
        (uri
          (git-reference
            (url "https://github.com/robx/pzpr-puzzlink.git")
            (commit "dist")))
        (sha256
          (base32
            "1bpmg71q3hwlkfpf7k167iw9h3jpxs0l0nn2vhfc1kaa7vi7idch"))))
    (build-system trivial-build-system)
    (inputs `(("pzprjs" ,pzprjs-local)))
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively
             (string-append (assoc-ref %build-inputs "source") "/dist")
             %output)
           (copy-recursively
             (string-append (assoc-ref %build-inputs "pzprjs") "/js")
             (string-append %output "/js"))
           #t)))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))
