(define-module (puzzle-draw)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
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
  #:use-module (ice-9 match)

  #:use-module (guix build-system elm)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)

  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell))

(define-public puzzle-draw
  (package
    (name "puzzle-draw")
    (version "current")
    (source "/home/rob/puzzle-draw")
    (build-system haskell-build-system)
    (inputs
     `(("ghc-diagrams-lib" ,ghc-diagrams-lib)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-svg-builder" ,ghc-svg-builder)
       ("ghc-svgfonts" ,ghc-svgfonts)
       ("ghc-vector-space" ,ghc-vector-space)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)
       ("ghc-linear" ,ghc-linear)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-diagrams-rasterific"
        ,ghc-diagrams-rasterific)
       ("ghc-diagrams-svg" ,ghc-diagrams-svg)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-snap-core" ,ghc-snap-core)
       ("ghc-snap-server" ,ghc-snap-server)
       ("ghc-safe" ,ghc-safe)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hspec" ,ghc-tasty-hspec)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-blaze-svg" ,ghc-blaze-svg)))
    (home-page
     "http://hackage.haskell.org/package/puzzle-draw")
    (synopsis
     "Creating graphics for pencil puzzles")
    (description
     "puzzle-draw is a library and tool for drawing pencil puzzles using Diagrams.
    It aims to provide a utility layer on top of Diagrams to help with drawing arbitrary
    puzzles, and to support several specific puzzles types In addition, it includes
    functionality for parsing puzzle data from a YAML file format.")
    (license license:expat)))

(define-public puzzle-draw-elm
  (let ((elm-virtual-dom
         (package
           (name "elm-virtual-dom")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/virtual-dom" version))
              (sha256
               (base32
                "0hm8g92h7z39km325dlnhk8n00nlyjkqp3r3jppr37k2k13md6aq"))))
           (build-system elm-package-build-system)
           (synopsis
            "Core virtual DOM implementation, basis for HTML and SVG libraries")
           (description
            "Core virtual DOM implementation, basis for HTML and SVG libraries")
           (home-page #f)
           (license license:bsd-3)))
        (elm-time
         (package
           (name "elm-time")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/time" version))
              (sha256
               (base32
                "0vch7i86vn0x8b850w1p69vplll1bnbkp8s383z7pinyg94cm2z1"))))
           (build-system elm-package-build-system)
           (synopsis
            "Work with POSIX times, time zones, years, months, days, hours, seconds, etc.")
           (description
            "Work with POSIX times, time zones, years, months, days, hours, seconds, etc.")
           (home-page #f)
           (license license:bsd-3)))
        (elm-core
         (package
           (name "elm-core")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/core" version))
              (sha256
               (base32
                "10kr86h4v5h4p0586q406a5wbl8xvr1jyrf6097zp2wb8sv21ylw"))))
           (build-system elm-package-build-system)
           (synopsis "Elm's standard libraries")
           (description "Elm's standard libraries")
           (home-page #f)
           (license license:bsd-3)))
        (elm-http
         (package
           (name "elm-http")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/http" version))
              (sha256
               (base32
                "1igmm89ialzrjib1j8xagkxalq1x2gj4l0hfxcd66mpwmvg7psl8"))))
           (build-system elm-package-build-system)
           (synopsis "Make HTTP requests")
           (description "Make HTTP requests")
           (home-page #f)
           (license license:bsd-3)))
        (elm-html
         (package
           (name "elm-html")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/html" version))
              (sha256
               (base32
                "1n3gpzmpqqdsldys4ipgyl1zacn0kbpc3g4v3hdpiyfjlgh8bf3k"))))
           (build-system elm-package-build-system)
           (synopsis
            "Fast HTML, rendered with virtual DOM diffing")
           (description
            "Fast HTML, rendered with virtual DOM diffing")
           (home-page #f)
           (license license:bsd-3)))
        (elm-json
         (package
           (name "elm-json")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/json" version))
              (sha256
               (base32
                "1g0hafkqf2q633r7ir9wxpb1lnlzskhpsyi0h5bkzj0gl072zfnb"))))
           (build-system elm-package-build-system)
           (synopsis "Encode and decode JSON values")
           (description "Encode and decode JSON values")
           (home-page #f)
           (license license:bsd-3)))
        (elm-url
         (package
           (name "elm-url")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/url" version))
              (sha256
               (base32
                "0av8x5syid40sgpl5vd7pry2rq0q4pga28b4yykn9gd9v12rs3l4"))))
           (build-system elm-package-build-system)
           (synopsis
            "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)")
           (description
            "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)")
           (home-page #f)
           (license license:bsd-3)))
        (elm-browser
         (package
           (name "elm-browser")
           (version "1.0.0")
           (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/browser" version))
              (sha256
               (base32
                "1apmvyax93nvmagwj00y16zx10kfv640cxpi64xgqbgy7d2wphy4"))))
           (build-system elm-package-build-system)
           (synopsis
            "Run Elm in browsers, with access to browser history for single-page apps (SPAs)")
           (description
            "Run Elm in browsers, with access to browser history for single-page apps (SPAs)")
           (home-page #f)
           (license license:bsd-3))))
    (package
      (name "puzzle-draw-elm")
      (version "current")
      (source "/home/rob/puzzle-draw/web")
      (build-system elm-application-build-system)
      (arguments `(#:elm-modules ((("Main.elm") . "web.js"))))
      (native-inputs
       `(("elm-virtual-dom" ,elm-virtual-dom)
         ("elm-time" ,elm-time)
         ("elm-core" ,elm-core)
         ("elm-http" ,elm-http)
         ("elm-html" ,elm-html)
         ("elm-json" ,elm-json)
         ("elm-url" ,elm-url)
         ("elm-browser" ,elm-browser)))
      (synopsis #f)
      (description #f)
      (home-page #f)
      (license #f))))

(define-public puzzle-draw-frontend
  (package
    (name "puzzle-draw-frontend")
    (version "current")
    (source "/home/rob/puzzle-draw")
    (build-system trivial-build-system)
    (inputs
     `(("puzzle-draw" ,puzzle-draw)
       ("bash" ,bash)))
    (native-inputs
     `(("puzzle-draw-elm" ,puzzle-draw-elm)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((elm (assoc-ref %build-inputs "puzzle-draw-elm"))
                (draw (assoc-ref %build-inputs "puzzle-draw"))
                (source (assoc-ref %build-inputs "source"))
                (bash (assoc-ref %build-inputs "bash"))
                (out (assoc-ref %outputs "out")))
           (copy-recursively (string-append source "/static") (string-append out "/static"))
           (copy-recursively (string-append source "/tests/examples") (string-append out "/tests/examples"))
           (copy-recursively elm (string-append out "/static"))
           (install-file
            (string-append draw "/bin/servepuzzle")
            (string-append out "/bin"))
           (setenv "PATH" (string-append (getenv "PATH") ":" bash "/bin"))
           (wrap-program
             (string-append out "/bin/servepuzzle")
             `("PUZZLE_DRAW_ROOT" ":" = (,out)))))))
    (description #f)
    (synopsis #f)
    (license #f)
    (home-page #f)))


;; todo: configure port
(define-record-type* <puzzle-draw-configuration>
  puzzle-draw-configuration make-puzzle-draw-configuration
  puzzle-draw-configuration?
  (puzzle-draw puzzle-draw-configuration-puzzle-draw (default puzzle-draw-frontend)))

(define %puzzle-draw-accounts
  (list (user-group (name "pzldraw") (system? #t))
        (user-account
         (name "pzldraw")
         (group "pzldraw")
         (system? #t)
         (comment "puzzle-draw server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

; copied over from postgrest
; FIXME: handle command line arguments
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

(define puzzle-draw-shepherd-service
  (match-lambda
    (($ <puzzle-draw-configuration> puzzle-draw-frontend)
     (list (shepherd-service
            (provision '(puzzle-draw))
            (documentation "Run the puzzle-draw daemon.")
            (requirement '(user-processes))
            (start #~(make-forkexec-constructor
                      '(#$(logger-wrapper "puzzle-draw" (file-append puzzle-draw-frontend "/bin/servepuzzle")
                                          "-b" "127.0.0.1" "-p" "8765"))
                      #:user "pzldraw"
                      #:group "pzldraw"))
            (stop #~(make-kill-destructor)))))))

(define-public puzzle-draw-service-type
  (service-type
   (name 'puzzle-draw)
   (extensions
    (list (service-extension shepherd-root-service-type
                             puzzle-draw-shepherd-service)
          (service-extension account-service-type
                             (const %puzzle-draw-accounts))))
   (default-value (puzzle-draw-configuration))))

(define-public ghc-svgfonts
  (package
    (name "ghc-svgfonts")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SVGFonts/SVGFonts-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1k9ili7l9pp5a009jh55vigb917wdnsl6iaz0ggp6d4nw1jwsg6s"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-cereal" ,ghc-cereal)
       ("ghc-cereal-vector" ,ghc-cereal-vector)
       ("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-diagrams-core" ,ghc-diagrams-core)
       ("ghc-diagrams-lib" ,ghc-diagrams-lib)
       ("ghc-blaze-svg" ,ghc-blaze-svg)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-split" ,ghc-split)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml" ,ghc-xml)))
    (arguments
     `(#:cabal-revision
       ("1"
        "1w687f4lk4l07wqgldhpg7ycid0fs099x8vrylcxqdgfrzmm04dg")))
    (home-page
     "http://hackage.haskell.org/package/SVGFonts")
    (synopsis "Fonts from the SVG-Font format")
    (description
     "Native font support for the diagrams framework (<http://projects.haskell.org/diagrams/>). Note that this package can be used with any diagrams backend, not just the SVG backend.  The SVG-font format is easy to parse and was therefore chosen for a font library completely written in Haskell. . You can convert your own font to SVG with <http://fontforge.github.io/>, or use the included LinLibertine and Bitstream fonts. . Features: . * Complete implementation of the features that Fontforge produces (though not the complete SVG format) . * Kerning (/i.e./ the two characters in \\\"VA\\\" are closer than the characters in \\\"VV\\\") . * Unicode . * Ligatures . * An example that shows how to do text boxes with syntax highlighting using highlighting-kate: <http://hackage.haskell.org/package/highlighting-kate> . XML speed issues can be solved by trimming the svg file to only those characters that are used (or maybe binary xml one day). . Version 1.0 of this library supports texturing, though this only makes sense in a diagrams backend that does rasterization in Haskell, such as diagrams-rasterific. . Example: . >  # LANGUAGE NoMonomorphismRestriction # > > main = do linLibertine <- loadDataFont \"fonts/LinLibertine.svg\" >           t <- text'''' linLibertine \"Hello\" >           mainWith (t :: Diagram B) > > text'   font t = stroke (textSVG t 1) # fc purple # fillRule EvenOdd > text''  font t = stroke (textSVG' (TextOpts font INSIDE_H KERN False 1 1) t) # fillRule EvenOdd > text''' font t =        (textSVG_ (TextOpts font INSIDE_H KERN True  1 1) t) # fillRule EvenOdd > > -- using a local font > text'''' font t = do >    font <- loadFont \"/path/to/font.svg\" >    return $ stroke (textSVG' (TextOpts font INSIDE_H KERN False 1 1) t) .")
    (license license:bsd-3)))

(define-public ghc-vector-space
  (package
    (name "ghc-vector-space")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/vector-space/vector-space-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17676s2f8i45dj5gk370nc8585aylah7m34nbf34al7r1492y2qc"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memotrie" ,ghc-memotrie)
       ("ghc-boolean" ,ghc-boolean)
       ("ghc-numinstances" ,ghc-numinstances)))
    (home-page
     "http://hackage.haskell.org/package/vector-space")
    (synopsis
     "Vector & affine spaces, linear maps, and derivatives")
    (description
     "/vector-space/ provides classes and generic operations for vector spaces and affine spaces.  It also defines a type of infinite towers of generalized derivatives.  A generalized derivative is a linear transformation rather than one of the common concrete representations (scalars, vectors, matrices, ...). . /Warning/: this package depends on type families working fairly well, requiring GHC version at least 6.9. . Project wiki page: <http://haskell.org/haskellwiki/vector-space> . &#169; 2008-2012 by Conal Elliott; BSD3 license.")
    (license license:bsd-3)))

(define-public ghc-diagrams-rasterific
  (package
    (name "ghc-diagrams-rasterific")
    (version "1.4.2-git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://github.com/diagrams/diagrams-rasterific.git")
             (commit "9a38b8888ff09842de603f2dc6e8ec55c0fe52e7")))
       (sha256
        (base32
         "0cq6ka6i05w3g5pfi4m8hkxrdwbfa1hzlns0crz303b7q5qihw24"))))
                                        ;(method url-fetch)
                                        ;(uri (string-append
                                        ;      "https://hackage.haskell.org/package/diagrams-rasterific/diagrams-rasterific-"
                                        ;      version
                                        ;      ".tar.gz"))
                                        ;(sha256
                                        ; (base32
                                        ;  "0raki8c20s40y5xy2ax8y38xl3y40fb9qv95ax3qgnmi46s8fapp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-diagrams-core" ,ghc-diagrams-core)
       ("ghc-diagrams-lib" ,ghc-diagrams-lib)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-rasterific" ,ghc-rasterific)
       ("ghc-fontyfruity" ,ghc-fontyfruity)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-lens" ,ghc-lens)
       ("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)
       ("ghc-file-embed" ,ghc-file-embed)))
    (home-page
     "http://projects.haskell.org/diagrams/")
    (synopsis "Rasterific backend for diagrams.")
    (description
     "A full-featured backend for rendering diagrams using the Rasterific rendering engine.")
    (license license:bsd-3)))

(define-public ghc-blaze-svg
  (package
    (name "ghc-blaze-svg")
    (version "0.3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/blaze-svg/blaze-svg-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0q5a4wam0sidng0cfsivwkyph9snyilk7rsdx4vb6wz9l6xz397n"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-markup" ,ghc-blaze-markup)))
    (home-page
     "https://github.com/deepakjois/blaze-svg")
    (synopsis "SVG combinator library")
    (description
     "A blazingly fast SVG combinator library for the Haskell programming language. The \"Text.Blaze.SVG\" module is a good starting point. . Other documentation: . * Programs in the /examples/ folder of this project: <https://github.com/deepakjois/blaze-svg/tree/master/examples/> . * Jasper Van Der Jeugt has written a tutorial for /blaze-html/, which is a sister library of /blaze-svg/. It may not be directly relevant, but still it gives a good overview on how to use the  combinators in \"Text.Blaze.Svg11\" and \"Text.Blaze.Svg11.Attributes\": <http://jaspervdj.be/blaze/tutorial.html>.")
    (license license:bsd-3)))

(define-public ghc-active
  (package
    (name "ghc-active")
    (version "0.2.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/active/active-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yw029rh0gb63bhwwjynbv173mny14is4cyjkrlvzvxwb0fi96jx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-lens" ,ghc-lens)
       ("ghc-linear" ,ghc-linear)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("8"
        "1j771jblfaygc3qf8iaw9b87yrqxhkq79mdi9zyhvlr2vcac362s")))
    (home-page
     "http://hackage.haskell.org/package/active")
    (synopsis "Abstractions for animation")
    (description
     "\"Active\" abstraction for animated things with finite start and end times.")
    (license license:bsd-3)))

(define-public ghc-cereal-vector
  (package
    (name "ghc-cereal-vector")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cereal-vector/cereal-vector-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0czrb4l1n73cfxxlzbcqfa34qa3gw0m5w5mlz0rawylyqfk8a1pz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-cereal" ,ghc-cereal)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page
     "https://github.com/acfoltzer/cereal-vector")
    (synopsis
     "Serialize instances for Data.Vector types.")
    (description "")
    (license license:bsd-3)))

(define-public ghc-boolean
  (package
    (name "ghc-boolean")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Boolean/Boolean-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1y7f8lqx86m06ccq1bjym2hywc7r17s2bvx16jswb2ibn09n08b7"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/Boolean")
    (synopsis "Generalized booleans and numbers")
    (description
     "Some classes for generalized boolean operations. Starting with 0.1.0, this package uses type families. Up to version 0.0.2, it used MPTCs with functional dependencies. My thanks to Andy Gill for suggesting & helping with the change. Thanks also to Alex Horsman for Data.Boolean.Overload and to Jan Bracker for Data.Boolean.Numbers. . Copyright 2009-2013 Conal Elliott; BSD3 license.")
    (license license:bsd-3)))

(define-public ghc-numinstances
  (package
    (name "ghc-numinstances")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/NumInstances/NumInstances-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ycnwn09izajv330l7a31mc0alifqmxjsn9qmfswwnbg6i4jmnyb"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/conal/NumInstances")
    (synopsis
     "Instances of numeric classes for functions and tuples")
    (description
     "Instances of numeric classes for functions and tuples. Import \"Data.NumInstances\" to get all the instances. If you want only function or only tuple instances, import \"Data.NumInstances.Function\" or \"Data.NumInstances.Tuple\".")
    (license license:bsd-3)))

(define-public ghc-rasterific
  (package
    (name "ghc-rasterific")
    (version "0.7.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Rasterific/Rasterific-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1s8d0yyh2xz8kb9476nr11jzxvgq0y9sgspgzhh9shf44fbz4gs0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-free" ,ghc-free)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-fontyfruity" ,ghc-fontyfruity)
       ("ghc-vector" ,ghc-vector)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector-algorithms" ,ghc-vector-algorithms)))
    (home-page
     "http://hackage.haskell.org/package/Rasterific")
    (synopsis "A pure haskell drawing engine.")
    (description
     "Rasterific is a vector drawing library (a rasterizer) implemented in pure haskell. . Can render vector graphics to an image and export vector data to PDF.")
    (license license:bsd-3)))

(define-public ghc-fontyfruity
  (package
    (name "ghc-fontyfruity")
    (version "0.5.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/FontyFruity/FontyFruity-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gavpjv83vg5q2x254d3zi3kw5aprl6z8ifcn0vs6hymaj0qgls3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector) ("ghc-xml" ,ghc-xml)))
    (home-page
     "http://hackage.haskell.org/package/FontyFruity")
    (synopsis "A true type file format loader")
    (description
     "A haskell Truetype file parser. . You can load a font file and extract some curves to be drawed with a library like Rasterific .")
    (license license:bsd-3)))
