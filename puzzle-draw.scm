(define-module (puzzle-draw)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
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

(define-public ghc-diagrams-lib
  (package
    (name "ghc-diagrams-lib")
    (version "1.4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/diagrams-lib/diagrams-lib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "175yzi5kw4yd8ykdkpf64q85c7j3p89l90m3h6qcsx9ipv6av9r5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-monoid-extras" ,ghc-monoid-extras)
       ("ghc-dual-tree" ,ghc-dual-tree)
       ("ghc-diagrams-core" ,ghc-diagrams-core)
       ("ghc-diagrams-solve" ,ghc-diagrams-solve)
       ("ghc-active" ,ghc-active)
       ("ghc-colour" ,ghc-colour)
       ("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-fingertree" ,ghc-fingertree)
       ("ghc-intervals" ,ghc-intervals)
       ("ghc-lens" ,ghc-lens)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-linear" ,ghc-linear)
       ("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-fsnotify" ,ghc-fsnotify)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-cereal" ,ghc-cereal)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-numeric-extras" ,ghc-numeric-extras)))
    (arguments
     `(#:cabal-revision
       ("2"
        "0gn1lpsq1v9qpyhpizyknn3sfixg1b64s0dsl1jf25lz4kcrpbs7")))
    (home-page
     "http://projects.haskell.org/diagrams")
    (synopsis
     "Embedded domain-specific language for declarative graphics")
    (description
     "Diagrams is a flexible, extensible EDSL for creating graphics of many types.
Graphics can be created in arbitrary vector spaces and rendered with multiple backends.
diagrams-lib provides a standard library of primitives and operations for creating diagrams.
To get started using it, see the Diagrams module, and refer to the tutorials and
documentation on the diagrams website, http://projects.haskell.org/diagrams.")
    (license license:bsd-3)))

(define-public ghc-svg-builder
  (package
    (name "ghc-svg-builder")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/svg-builder/svg-builder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1k420f497lzkymmxin88ql6ib8dziic43avykv31yq65rgrf7l2g"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)))
    (arguments
     `(#:cabal-revision
       ("1"
        "1bhp9gvid2iis411k1vvyj5krzc4ahxcqcd9cwx9h37jxg180xw1")))
    (home-page
     "https://github.com/diagrams/svg-builder.git")
    (synopsis "DSL for building SVG.")
    (description "Fast, easy to write SVG.")
    (license license:bsd-3)))

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
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/vector-space/vector-space-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05yn93vnhzhpp2i6qb4b3dasvmpk71rab6vhssqvpb3qhdvxb482"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memotrie" ,ghc-memotrie)
       ("ghc-boolean" ,ghc-boolean)
       ("ghc-numinstances" ,ghc-numinstances)))
    (arguments
     `(#:cabal-revision
       ("2"
        "1p9vibym0ggr1rjyak0wphswdl4vik2b2w85afgvyj9zn32w28bw")))
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

(define-public ghc-diagrams-svg
  (package
    (name "ghc-diagrams-svg")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/diagrams-svg/diagrams-svg-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1lnyxx45yawqas7hmvvannwaa3ycf1l9g40lsl2m8sl2ja6vcmal"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-colour" ,ghc-colour)
       ("ghc-diagrams-core" ,ghc-diagrams-core)
       ("ghc-diagrams-lib" ,ghc-diagrams-lib)
       ("ghc-monoid-extras" ,ghc-monoid-extras)
       ("ghc-svg-builder" ,ghc-svg-builder)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-split" ,ghc-split)
       ("ghc-lens" ,ghc-lens)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)
       ("ghc-semigroups" ,ghc-semigroups)))
    (arguments
     `(#:cabal-revision
       ("2"
        "15sn85xaachw4cj56w61bjcwrbf4qmnkfl8mbgdapxi5k0y4f2qv")))
    (home-page
     "http://projects.haskell.org/diagrams/")
    (synopsis
     "SVG backend for diagrams drawing EDSL.")
    (description
     "This package provides a modular backend for rendering diagrams created with the diagrams EDSL to SVG files.  It uses @lucid-svg@ to be a native Haskell backend, making it suitable for use on any platform. . The package provides the following modules: . * \"Diagrams.Backend.SVG.CmdLine\" - if you're just getting started with diagrams, begin here. . * \"Diagrams.Backend.SVG\" - look at this next. The general API for the SVG backend. . Additional documentation can be found in the README file distributed with the source tarball or viewable on GitHub: <https://github.com/diagrams/diagrams-svg/blob/master/README.md>.")
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

(define-public ghc-monoid-extras
  (package
    (name "ghc-monoid-extras")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/monoid-extras/monoid-extras-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "172d1mfns7agd619rlbb1i9kw2y26kjvivkva06k1r14bar1lmy6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-groups" ,ghc-groups)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-semigroupoids" ,ghc-semigroupoids)))
    (arguments
     `(#:cabal-revision
       ("2"
        "1q73ghd12fd451zm4m045h8v3y61jmfhj6k890gnv6z7lyb7xwg2")))
    (home-page
     "http://hackage.haskell.org/package/monoid-extras")
    (synopsis
     "Various extra monoid-related definitions and utilities")
    (description
     "Various extra monoid-related definitions and utilities, such as monoid actions, monoid coproducts, semi-direct products, \\\"deletable\\\" monoids, \\\"split\\\" monoids, and \\\"cut\\\" monoids.")
    (license license:bsd-3)))

(define-public ghc-dual-tree
  (package
    (name "ghc-dual-tree")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/dual-tree/dual-tree-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1sx9p9yr06z7bi7pshjpswizs6bkmfzcpw8xlasriniry86df4kl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-newtype-generics" ,ghc-newtype-generics)
       ("ghc-monoid-extras" ,ghc-monoid-extras)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-testing-feat" ,ghc-testing-feat)))
    (arguments
     `(#:cabal-revision
       ("2"
        "0r8idr1haqixa9nlp8db5iw9vr9sdk6rcargkr7w7s6i99lm6jmh")))
    (home-page
     "http://hackage.haskell.org/package/dual-tree")
    (synopsis
     "Rose trees with cached and accumulating monoidal annotations")
    (description
     "Rose (n-ary) trees with both upwards- (/i.e./ cached) and downwards-traveling (/i.e./ accumulating) monoidal annotations.  This is used as the core data structure underlying the @diagrams@ framework (<http://projects.haskell.org/diagrams>), but potentially has other applications as well. . Abstractly, a DUALTree is a rose (n-ary) tree with data (of type @l@) at leaves, data (of type @a@) at internal nodes, and two types of monoidal annotations, one (of type @u@) travelling \\\"up\\\" the tree and one (of type @d@) traveling \\\"down\\\". . See \"Data.Tree.DUAL\" for full documentation. \"Data.Tree.DUAL\" provides a public API which should suffice for most purposes. \"Data.Tree.DUAL.Internal\" exports more of the internal implementation---use it at your own risk.")
    (license license:bsd-3)))

(define-public ghc-diagrams-core
  (package
    (name "ghc-diagrams-core")
    (version "1.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/diagrams-core/diagrams-core-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "10mnicfyvawy3jlpgf656fx2y4836x04p3z1lpgyyr1nkvwyk0m1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-monoid-extras" ,ghc-monoid-extras)
       ("ghc-dual-tree" ,ghc-dual-tree)
       ("ghc-lens" ,ghc-lens)
       ("ghc-linear" ,ghc-linear)
       ("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-profunctors" ,ghc-profunctors)))
    (arguments
     `(#:cabal-revision
       ("2"
        "1lf7xcq42l4hjksgp1nhj7600shvw9q5a27bh729fyfphmvv3xkf")))
    (home-page
     "http://projects.haskell.org/diagrams")
    (synopsis "Core libraries for diagrams EDSL")
    (description
     "The core modules underlying diagrams, an embedded domain-specific language for compositional, declarative drawing.")
    (license license:bsd-3)))

(define-public ghc-diagrams-solve
  (package
    (name "ghc-diagrams-solve")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/diagrams-solve/diagrams-solve-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17agchqkmj14b17sw50kzxq4hm056g5d8yy0wnqn5w8h1d0my7x4"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("4"
        "1yjacw17ga4rh6iw70vclk03qm5xjw4y17c7m43gjw8h3cfaq15d")))
    (home-page
     "http://projects.haskell.org/diagrams")
    (synopsis
     "Pure Haskell solver routines used by diagrams")
    (description
     "Pure Haskell solver routines used by the diagrams project.  Currently includes finding real roots of low-degree (n < 5) polynomials, and solving tridiagonal and cyclic tridiagonal linear systems.")
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

(define-public ghc-intervals
  (package
    (name "ghc-intervals")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/intervals/intervals-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "00vyxf3ba9d7aas3npfapr53w71fslgh69fczjb25axr66fvzqww"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-distributive" ,ghc-distributive)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("cabal-doctest" ,cabal-doctest)))
    (arguments
     `(#:cabal-revision
       ("4"
        "1qx3q0v13l1zaln9zdk8chxpxhshbz5x0vqm0qda7d1kpv7h6a7r")))
    (home-page "http://github.com/ekmett/intervals")
    (synopsis "Interval Arithmetic")
    (description
     "A 'Numeric.Interval.Interval' is a closed, convex set of floating point values. . We do not control the rounding mode of the end points of the interval when using floating point arithmetic, so be aware that in order to get precise containment of the result, you will need to use an underlying type with both lower and upper bounds like 'CReal'")
    (license license:bsd-3)))

(define-public ghc-numeric-extras
  (package
    (name "ghc-numeric-extras")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/numeric-extras/numeric-extras-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1mk11c0gz1yjy5b8dvq6czfny57pln0bs7x28fz38qyr44872067"))))
    (build-system haskell-build-system)
    (home-page
     "http://github.com/ekmett/numeric-extras")
    (synopsis
     "Useful tools from the C standard library")
    (description
     "Useful tools from the C standard library")
    (license license:bsd-3)))

(define-public ghc-groups
  (package
    (name "ghc-groups")
    (version "0.4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/groups/groups-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ggkygkyxw5ga4cza82bjvdraavl294k0h6b62d2px7z3nvqhifx"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/groups")
    (synopsis "Haskell 98 groups")
    (description
     "Haskell 98 groups. A group is a monoid with invertibility.")
    (license license:bsd-3)))

(define-public ghc-testing-feat
  (package
    (name "ghc-testing-feat")
    (version "1.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/testing-feat/testing-feat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1v2qzzpf1s008g7q6q67glf7vbm1pkpq4rc3ii74f4g6vhfx610r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-size-based" ,ghc-size-based)
       ("ghc-testing-type-modifiers"
        ,ghc-testing-type-modifiers)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page
     "https://github.com/JonasDuregard/testing-feat")
    (synopsis
     "Functional Enumeration of Algebraic Types")
    (description
     "Feat (Functional Enumeration of Algebraic Types) provides enumerations as functions from natural numbers to values (similar to @toEnum@ but for any algebraic data type). This can be used for SmallCheck-style systematic testing, QuickCheck style random testing, and hybrids of the two. . The enumerators are defined in a very boilerplate manner and there is a Template Haskell script for deriving the class instance for most types. \"Test.Feat\" contain a subset of the other modules that should be sufficient for most test usage. There are some small and large example in the tar ball. . The generators are provided by the size-based package. This means other libraries that implement the Sized class can use the same generator definitions. One such is the <https://hackage.haskell.org/package/lazy-search lazy-search package>, that uses laziness to search for values and test properties. This is typically a lot faster than Feat for properties that have preconditions (logical implication), but can not be used for random selection of values.")
    (license license:bsd-3)))

(define-public ghc-size-based
  (package
    (name "ghc-size-based")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/size-based/size-based-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06hmlic0n73ncwlkpx49xlv09bzsrr27ncnp5byhzlknak2gd7vp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-dictionary-sharing"
        ,ghc-dictionary-sharing)
       ("ghc-testing-type-modifiers"
        ,ghc-testing-type-modifiers)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page
     "http://hackage.haskell.org/package/size-based")
    (synopsis
     "Sized functors, for size-based enumerations")
    (description
     "A framework for size-based enumerations. See the module documentations for details.")
    (license license:bsd-3)))

(define-public ghc-testing-type-modifiers
  (package
    (name "ghc-testing-type-modifiers")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/testing-type-modifiers/testing-type-modifiers-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wh2n95n39ivv6kbqn42vbzrj8zagsmk6f2al2qj40bg5kgdl2q5"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/testing-type-modifiers")
    (synopsis
     "Data type modifiers for property based testing")
    (description
     "Property based testing libraries such as QuickCheck tend to include type modifiers. Most of them are used to quantify over subsets of a type. For example a property on non-empty lists: . @  prop_tail_length (NonEmpty xs) = length (tail xs) == length xs - 1 @ . This library is intended to supply these modifiers to be used by testing libraries, in an effort to make properties more portable between testing frameworks. . For every modifier it also provides an access function that converts to the underlying type, which enables point-free style properties as such: . @ prop_tail_length2 = (> 0) . length . nonEmpty @")
    (license license:public-domain)))

(define-public ghc-dictionary-sharing
  (package
    (name "ghc-dictionary-sharing")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/dictionary-sharing/dictionary-sharing-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "00aspv943qdqhlk39mbk00kb1dsa5r0caj8sslrn81fnsn252fwc"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("3"
        "1mn7jcc7h3b8f1pn9zigqp6mc2n0qb66lms5qnrx4zswdv5w9439")))
    (home-page
     "http://hackage.haskell.org/package/dictionary-sharing")
    (synopsis "Sharing/memoization of class members")
    (description
     "Library for ensuring that class members are shared.")
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
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Rasterific/Rasterific-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "13f5ay9wmva9k15a6pk4imxz6rj80gwc1f16906m7a6rm9vgwvlq"))))
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
