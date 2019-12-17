(define-module (puzzledb-elm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system elm)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (json))

(define-public puzzledb-frontend
 (package
   (name "puzzledb-frontend")
   (version "current")
   (source "/home/rob/puzzledb/frontend")
   (build-system trivial-build-system)
   (native-inputs
    `(("puzzledb-elm" ,puzzledb-elm)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
        (begin
          (use-modules (guix build utils))
          (let* ((elm (assoc-ref %build-inputs "puzzledb-elm"))
                 (source (assoc-ref %build-inputs "source"))
                 (out (assoc-ref %outputs "out")))
            (copy-recursively elm out)
            (for-each
              (lambda (f) (copy-file (string-append source "/" f) (string-append out "/" f)))
              '("style.css" "pzvs.html" "blogs.html" "puzzle.html" "gmpuzzles.html" "gmsketch.html"))
            (symlink "pzvs.html" (string-append out "/index.html"))))))
   (description #f)
   (synopsis #f)
   (license #f)
   (home-page #f)))

(define-public puzzledb-elm
  (let
     ((elm-regex
        (package
          (name "elm-regex")
          (version "1.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/regex" version))
              (sha256
                (base32
                  "0lijsp50w7n1n57mjg6clpn9phly8vvs07h0qh2rqcs0f1jqvsa2"))))
          (build-system elm-package-build-system)
          (synopsis
            "Support for JS-style regular expressions in Elm")
          (description
            "Support for JS-style regular expressions in Elm")
          (home-page #f)
          (license license:bsd-3)))
      (elm-svg
        (package
          (name "elm-svg")
          (version "1.0.1")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/svg" version))
              (sha256
                (base32
                  "1cwcj73p61q45wqwgqvrvz3aypjyy3fw732xyxdyj6s256hwkn0k"))))
          (build-system elm-package-build-system)
          (synopsis
            "Fast SVG, rendered with virtual DOM diffing")
          (description
            "Fast SVG, rendered with virtual DOM diffing")
          (home-page #f)
          (license license:bsd-3)))
      (elm-list-extra
        (package
          (name "elm-list-extra")
          (version "8.1.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri
                     "elm-community/list-extra"
                     version))
              (sha256
                (base32
                  "0ngynq305b45zkz0398z66ldb1xl3jjnzdrj8fddim1zxjv2jdiw"))))
          (build-system elm-package-build-system)
          (synopsis
            "Convenience functions for working with List")
          (description
            "Convenience functions for working with List")
          (home-page #f)
          (license license:bsd-3)))
      (elm-virtual-dom
        (package
          (name "elm-virtual-dom")
          (version "1.0.2")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/virtual-dom" version))
              (sha256
                (base32
                  "0q1v5gi4g336bzz1lgwpn5b1639lrn63d8y6k6pimcyismp2i1yg"))))
          (build-system elm-package-build-system)
          (synopsis
            "Core virtual DOM implementation, basis for HTML and SVG libraries")
          (description
            "Core virtual DOM implementation, basis for HTML and SVG libraries")
          (home-page #f)
          (license license:bsd-3)))
      (elm-parser
        (package
          (name "elm-parser")
          (version "1.1.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/parser" version))
              (sha256
                (base32
                  "0a3cxrvbm7mwg9ykynhp7vjid58zsw03r63qxipxp3z09qks7512"))))
          (build-system elm-package-build-system)
          (synopsis
            "a parsing library, focused on simplicity and great error messages")
          (description
            "a parsing library, focused on simplicity and great error messages")
          (home-page #f)
          (license license:bsd-3)))
      (elm-random
        (package
          (name "elm-random")
          (version "1.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/random" version))
              (sha256
                (base32
                  "138n2455wdjwa657w6sjq18wx2r0k60ibpc4frhbqr50sncxrfdl"))))
          (build-system elm-package-build-system)
          (synopsis
            "Generate random numbers and values (RNG)")
          (description
            "Generate random numbers and values (RNG)")
          (home-page #f)
          (license license:bsd-3)))
      (elm-file
        (package
          (name "elm-file")
          (version "1.0.1")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/file" version))
              (sha256
                (base32
                  "15vw1ilbg0msimq2k9magwigp8lwqrgvz3vk6qia6b3ldahvw8jr"))))
          (build-system elm-package-build-system)
          (synopsis
            "Select files. Download files. Work with file content.")
          (description
            "Select files. Download files. Work with file content.")
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
      (elm-bytes
        (package
          (name "elm-bytes")
          (version "1.0.7")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/bytes" version))
              (sha256
                (base32
                  "040d7irrawcbnq9jxhzx8p9qacdlw5bncy6lgndd6inm53rvvwbp"))))
          (build-system elm-package-build-system)
          (synopsis
            "Work with sequences of bytes (a.k.a. ArrayBuffer, typed arrays, DataView)")
          (description
            "Work with sequences of bytes (a.k.a. ArrayBuffer, typed arrays, DataView)")
          (home-page #f)
          (license license:bsd-3)))
      (elm-hex
        (package
          (name "elm-hex")
          (version "1.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "rtfeldman/elm-hex" version))
              (sha256
                (base32
                  "1y0aa16asvwdqmgbskh5iba6psp43lkcjjw9mgzj3gsrg33lp00d"))))
          (build-system elm-package-build-system)
          (synopsis "Work with hexadecimal numbers")
          (description "Work with hexadecimal numbers")
          (home-page #f)
          (license license:bsd-3)))
      (elm-core
        (package
          (name "elm-core")
          (version "1.0.2")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/core" version))
              (sha256
                (base32
                  "1l0qdbczw91kzz8sx5d5zwz9x662bspy7p21dsr3f2rigxiix2as"))))
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
          (version "1.1.2")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/json" version))
              (sha256
                (base32
                  "1a107nmm905dih4w4mjjkkpdcjbgaf5qjvr7fl30kkpkckfjjnrw"))))
          (build-system elm-package-build-system)
          (synopsis "Encode and decode JSON values")
          (description "Encode and decode JSON values")
          (home-page #f)
          (license license:bsd-3)))
      (elm-sha256
        (package
          (name "elm-sha256")
          (version "1.0.8")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri
                     "billstclair/elm-sha256"
                     version))
              (sha256
                (base32
                  "0n13iw8hfz9fknfjjw5dd6p5xqb9rksjs3c59dnb0yaa7dag467f"))))
          (build-system elm-package-build-system)
          (synopsis
            "SHA256 and SHA228 cryptographic hashes Elm.")
          (description
            "SHA256 and SHA228 cryptographic hashes Elm.")
          (home-page #f)
          (license license:expat)))
      (elm-collage
        (package
          (name "elm-collage")
          (version "2.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "timjs/elm-collage" version))
              (sha256
                (base32
                  "1nwdvvkm7jgy0z9zw72x4kvadg6mjf79fkj2cbwdq2lh4zdqwa4y"))))
          (build-system elm-package-build-system)
          (synopsis
            "Create interactive vector graphics and position them relative to each other")
          (description
            "Create interactive vector graphics and position them relative to each other")
          (home-page #f)
          (license license:bsd-3)))
      (elm-markdown
        (package
          (name "elm-markdown")
          (version "1.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri
                     "elm-explorations/markdown"
                     version))
              (sha256
                (base32
                  "0k3110ixa4wwf3vkkdplagwah9ypr965qxr1y147rnsc1xsxmr6y"))))
          (build-system elm-package-build-system)
          (synopsis "Fast markdown parsing and rendering")
          (description
            "Fast markdown parsing and rendering")
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
      (elm-pointer-events
        (package
          (name "elm-pointer-events")
          (version "4.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri
                     "mpizenberg/elm-pointer-events"
                     version))
              (sha256
                (base32
                  "1wh0xvs17c43wjmilv6hysqnjk8hqnjp69dhjvqpy9fa92hisl5k"))))
          (build-system elm-package-build-system)
          (synopsis
            "Mouse, Touch, Pointer, Wheel and Drag events")
          (description
            "Mouse, Touch, Pointer, Wheel and Drag events")
          (home-page #f)
          (license license:mpl2.0)))
      (elm-html-parser
        (package
          (name "elm-html-parser")
          (version "2.2.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "hecrj/html-parser" version))
              (sha256
                (base32
                  "06dl1lgwf0c2z8lm8w443qbvldidx0dj0q8r5j6308mrj7blsm9d"))))
          (build-system elm-package-build-system)
          (synopsis "Parse HTML 5 in Elm")
          (description "Parse HTML 5 in Elm")
          (home-page #f)
          (license license:bsd-3)))
      (elm-color
        (package
          (name "elm-color")
          (version "1.0.1")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "the-sett/elm-color" version))
              (sha256
                (base32
                  "0v5pir6pvbg8z98li8z66pv7rz8nv00b6zbic6ly6v9q8fqwx8da"))))
          (build-system elm-package-build-system)
          (synopsis "A simple color module for Elm.")
          (description "A simple color module for Elm.")
          (home-page #f)
          (license license:bsd-3)))
      (elm-browser
        (package
          (name "elm-browser")
          (version "1.0.1")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "elm/browser" version))
              (sha256
                (base32
                  "1zlmx672glg7fdgkvh5jm47y85pv7pdfr5mkhg6x7ar6k000vyka"))))
          (build-system elm-package-build-system)
          (synopsis
            "Run Elm in browsers, with access to browser history for single-page apps (SPAs)")
          (description
            "Run Elm in browsers, with access to browser history for single-page apps (SPAs)")
          (home-page #f)
          (license license:bsd-3)))
      (elm-datetime
        (package
          (name "elm-datetime")
          (version "1.0.0")
          (source
            (origin
              (method url-fetch)
              (uri (elm-package-uri "robx/elm-datetime" version))
              (sha256
                (base32
                  "0w8y4s07i15dnj5wc9jnplm0q6rqmgixfcfw7i35nip68wfxc9n7"))))
          (build-system elm-package-build-system)
          (synopsis
            "Format localized date/time")
          (description
            "Format localized date/time")
          (home-page #f)
          (license license:bsd-3))))
  (package
    (name "puzzledb-elm")
    (version "current")
    (source "/home/rob/puzzledb/frontend")
    (build-system elm-application-build-system)
    (native-inputs
      `(("elm-regex" ,elm-regex)
        ("elm-svg" ,elm-svg)
        ("elm-list-extra" ,elm-list-extra)
        ("elm-virtual-dom" ,elm-virtual-dom)
        ("elm-parser" ,elm-parser)
        ("elm-random" ,elm-random)
        ("elm-file" ,elm-file)
        ("elm-time" ,elm-time)
        ("elm-bytes" ,elm-bytes)
        ("elm-hex" ,elm-hex)
        ("elm-core" ,elm-core)
        ("elm-http" ,elm-http)
        ("elm-html" ,elm-html)
        ("elm-json" ,elm-json)
        ("elm-sha256" ,elm-sha256)
        ("elm-collage" ,elm-collage)
        ("elm-markdown" ,elm-markdown)
        ("elm-url" ,elm-url)
        ("elm-pointer-events" ,elm-pointer-events)
        ("elm-html-parser" ,elm-html-parser)
        ("elm-color" ,elm-color)
        ("elm-browser" ,elm-browser)
        ("elm-datetime" ,elm-datetime)))
    (arguments `(#:elm-modules ((("Pzvs.elm") . "pzvs.js")
                                (("Puzzle.elm") . "puzzle.js")
                                (("Blogs.elm") . "blogs.js")
                                (("Gmpuzzles.elm") . "gmpuzzles.js")
                                (("Gmsketch.elm") . "gmsketch.js"))))
    (synopsis #f)
    (description #f)
    (home-page #f)
    (license #f))))
