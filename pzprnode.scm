(define-module (pzprnode)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages node)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages gnome) ;librsvg
  #:use-module (guix build-system trivial)

  #:use-module (pzpr)
  #:use-module (puzzlink))

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

(define-record-type* <pzprnode-configuration>
  pzprnode-configuration make-pzprnode-configuration
  pzprnode-configuration?
  (pzprnod pzprnode-configuration-pzprnode (default pzprnode)))

(define %pzprnode-accounts
  (list (user-group (name "pzprnode") (system? #t))
        (user-account
         (name "pzprnode")
         (group "pzprnode")
         (system? #t)
         (comment "pzprnode server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

; copied over from postgrest
(define* (logger-wrapper name exec . args)
  "Return a derivation that builds a script to start a process with
standard output and error redirected to syslog via logger."
  (define exp
    #~(begin
        (use-modules (ice-9 popen))
        (let* ((pid    (number->string (getpid)))
               (logger #$(file-append inetutils "/bin/logger"))
               (args   (list "-t" #$name (string-append "--id=" pid)))
               (pipe   (apply open-pipe* OPEN_WRITE logger args)))
          (dup pipe 1)
          (dup pipe 2)
          (execl #$exec #$exec #$@args))))
  (program-file (string-append name "-logger") exp))

(define pzprnode-shepherd-service
  (match-lambda
   (($ <pzprnode-configuration> pzpr-node)
    (list (shepherd-service
          (provision '(pzprnode))
          (documentation "Run the pzprnode daemon.")
          (requirement '(user-processes))
          (start #~(make-forkexec-constructor
                    '(#$(logger-wrapper "pzprnode" (file-append pzpr-node "/bin/pzprnode")))
                    #:user "pzprnode"
                    #:group "pzprnode"))
          (stop #~(make-kill-destructor)))))))

(define-public pzprnode-service-type
  (service-type
   (name 'pzprnode)
   (extensions
    (list (service-extension shepherd-root-service-type
                             pzprnode-shepherd-service)
          (service-extension account-service-type
                             (const %pzprnode-accounts))))
   (default-value (pzprnode-configuration))))
