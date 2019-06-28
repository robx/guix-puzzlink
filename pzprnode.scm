(define-module (pzprnode)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages node)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages gnome) ;librsvg
  #:use-module (guix build-system trivial))

(define-public pzprnode
  (package
    (name "pzprnode")
    (version "git-dist")
    (source
      (origin
        (method git-fetch)
        (file-name (git-file-name name version))
        (uri
          (git-reference
            (url "https://github.com/robx/pzprnode.git")
            (commit "master")))
        (sha256
          (base32
            "1r2c0nj01jqwlb741a0jb8h3jj4nac6yz0xmc82k39nzwvsnj17x"))))
    (build-system trivial-build-system)
    (inputs
      `(("node" ,node)
        ("graphicsmagick" ,graphicsmagick)
        ("librsvg" ,librsvg)
        ("bash" ,bash)
        ("pzprjs" ,pzprjs)
        ("pzpr-puzzlink" ,pzpr-puzzlink)))
    (arguments
      `(#:modules ((guix build utils))
        #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  (bin (string-append out "/bin"))
                  (source (assoc-ref %build-inputs "source"))
                  (node (assoc-ref %build-inputs "node"))
                  (pzpr (assoc-ref %build-inputs "pzpr-puzzlink"))
                  (pzprjs (assoc-ref %build-inputs "pzprjs"))
                  (bash (assoc-ref %build-inputs "bash"))
                  (gm (assoc-ref %build-inputs "graphicsmagick"))
                  (rsvg (assoc-ref %build-inputs "librsvg")))
             (copy-recursively
               (string-append source "/templates")
               (string-append out "/templates"))
             (install-file
               (string-append source "/pzprnode")
               bin)
           (patch-shebang
             (string-append bin "/pzprnode")
             (list (string-append node "/bin")))
           (setenv "PATH" (string-append (getenv "PATH") ":" bash "/bin"))
           (wrap-program
             (string-append bin "/pzprnode")
             `("PATH" ":" prefix (,(string-append gm "/bin")
                                  ,(string-append rsvg "/bin")))
             `("NODE_PATH" ":" prefix (,(string-append pzprjs "/js")))
             `("TEMPLATE_DIR" ":" = (,(string-append out "/templates")))
             `("PZPR_DIR" ":" = (,pzpr))))
           #t)))
    (home-page #f)
    (synopsis #f)
    (license #f)
    (description #f)))

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
            (commit "dist")))
        (sha256
          (base32
            "0ln8vsnm0cj684h98glfww4bmhm61d2v1bhckjqda80n2d1906yx"))))
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
