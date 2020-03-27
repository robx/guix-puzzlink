(define-module (puzzld)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

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
