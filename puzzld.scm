(define-module (puzzld)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix records)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (ice-9 match)

  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell))

(define-public puzzld
  (package
    (name "puzzld")
    (version "current")
    (source "/home/rob/puzzld")
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-rio" ,ghc-rio)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-websockets" ,ghc-wai-websockets)
       ("ghc-warp" ,ghc-warp)
       ("ghc-websockets" ,ghc-websockets)))
    (home-page
     "https://github.com/robx/puzzld")
    (synopsis
     "Websocket puzzle server")
    (description
     "Websocket puzzle server")
    (license license:expat)))

(define-record-type* <puzzld-configuration>
  puzzld-configuration make-puzzld-configuration
  puzzld-configuration?
  (puzzld puzzld-configuration-puzzld (default puzzld)))

(define %puzzld-accounts
  (list (user-group (name "puzzld") (system? #t))
        (user-account
         (name "puzzld")
         (group "puzzld")
         (system? #t)
         (comment "puzzld server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

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

(define puzzld-shepherd-service
  (match-lambda
    (($ <puzzld-configuration> puzzld)
     (list (shepherd-service
            (provision '(puzzld))
            (documentation "Run the puzzld pzprjs websocket daemon.")
            (requirement '(user-processes))
            (start #~(make-forkexec-constructor
                      '(#$(logger-wrapper "puzzld" (file-append puzzld "/bin/puzzld")))
                      #:user "puzzld"
                      #:group "puzzld"))
            (stop #~(make-kill-destructor)))))))

(define-public puzzld-service-type
  (service-type
   (name 'puzzld)
   (extensions
    (list (service-extension shepherd-root-service-type
                             puzzld-shepherd-service)
          (service-extension account-service-type
                             (const %puzzld-accounts))))
   (default-value (puzzld-configuration))))
