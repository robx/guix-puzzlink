(define-module (puzzledb-tools)
  #:use-module (gnu packages golang)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go))

(define-public puzzledb-tools
  (package
    (name "puzzledb-tools")
    (version "current")
    (source "/home/rob/puzzledb/tools")
    (build-system go-build-system)
    (arguments
      `(#:import-path "gitlab.com/rrrob/puzzledb/tools"
        #:tests? #f
        #:install-source? #f
        #:phases
        (modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (invoke "make" "-C" (string-append "src/" import-path) "build")))
          (replace 'install
            (lambda* (#:key import-path outputs #:allow-other-keys)
              (invoke "make" "-C" (string-append "src/" import-path) "install"
                                  (string-append "dest=" (assoc-ref outputs "out") "/bin")))))))
    (native-inputs
      `(("go-gofeed" ,go-gofeed)
        ("go-x-net" ,go-x-net)
        ("go-imaging" ,go-imaging)
        ("go-colly", go-colly)
        ("go-x-oauth2" ,go-x-oauth2)))
    (synopsis "puzzledb tools")
    (license #f)
    (description "puzzledb tools")
    (home-page "https://puzz.link/db")))

(define-public go-gofeed
  (package
    (name "go-gofeed")
    (version "v1.0.0-beta2")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/mmcdole/gofeed")
          (commit version)))
      (sha256
        (base32
          "13bxah5fl6c7qrd55mxgn11dm22c999pxwycixw2cqa00h05j9xb"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-goquery" ,go-goquery)
        ("go-goxpp" ,go-goxpp)))
    (arguments
      `(#:import-path "github.com/mmcdole/gofeed"
        #:tests? #f))
    (synopsis "gofeed")
    (description "gofeed")
    (home-page "https://github.com/mmcdole/gofeed")
    (license #f)))

(define-public go-goxpp
  (package
    (name "go-goxpp")
    (version "v0.0.0-20181012175147-0068e33feabf")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/mmcdole/goxpp")
          (commit "0068e33feabf")))
      (sha256
        (base32
          "09b2birq4fdsknj266c0rydl7zgxxw1vimrkyahmksinvkp3y4sk"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/mmcdole/goxpp"
        #:tests? #f)) ; avoid dependencies
    (synopsis "goxpp")
    (description "goxpp")
    (home-page "https://github.com/mmcdole/goxpp")
    (license #f)))

(define-public go-goquery
  (package
    (name "go-goquery")
    (version "v1.5.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/PuerkitoBio/goquery")
          (commit version)))
      (sha256
        (base32
          "1fqf4rs66wy02nxz6w4mvs2qawf2j8srz17i294v64y8gvxisp56"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-cascadia" ,go-cascadia)))
    (arguments
      `(#:import-path "github.com/PuerkitoBio/goquery"))
    (synopsis "goquery")
    (description "goquery")
    (home-page "https://github.com/PuerkitoBio/goquery")
    (license #f)))

(define-public go-cascadia
  (package
    (name "go-cascadia")
    (version "v1.0.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/andybalholm/cascadia")
          (commit version)))
      (sha256
        (base32
          "09j8cavbhqqdxjqrkwbc40g8p0i49zf3184rpjm5p2rjbprcghcc"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-x-net" ,go-x-net)))
    (arguments
      `(#:import-path "github.com/andybalholm/cascadia"))
    (synopsis "gocascadia")
    (description "gocascadia")
    (home-page "https://github.com/andybalholm/cascadiia")
    (license #f)))

(define-public go-x-net
  (package
    (name "go-x-net")
    (version "v0.0.0-20190213061140-3a22650c66bd")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "https://go.googlesource.com/net")
          (commit "3a22650c66bd")))
      (sha256
        (base32
          "12q6fgdkc0742vcms67nzadgdwvi3rnb1gx3vwkljhrvlnwvz7wc"))))
    (build-system go-build-system)
    (propagated-inputs
       `(("go-x-text" ,go-x-text)))
    (arguments
      `(#:import-path "golang.org/x/net"
        #:unpack-path "golang.org/x/net"
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'generate-go-file
            (lambda _
              (call-with-output-file "src/golang.org/x/net/net.go"
                (lambda (port)
                  (format port "package net"))))))))
    (synopsis "x/net")
    (description "x/net")
    (home-page #f)
    (license #f)))

(define-public go-x-text
  (package
    (name "go-x-text")
    (version "v0.3.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "https://go.googlesource.com/text")
          (commit version)))
      (sha256
        (base32
          "0r6x6zjzhr8ksqlpiwm5gdd7s209kwk5p4lw54xjvz10cs3qlq19"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "golang.org/x/text"
        #:unpack-path "golang.org/x/text"))
    (synopsis "x/text/encoding")
    (description "x/text/encoding")
    (home-page #f)
    (license #f)))

(define-public go-x-image
  (package
    (name "go-x-image")
    (version "v0.0.0-20191009234506-e7c1f5e7dbb8")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "https://go.googlesource.com/image")
          (commit "e7c1f5e7dbb8")))
      (sha256
        (base32
          "0czp897aicqw1dgybj0hc2zzwb20rhqkdqm7siqci3yk7yk9cymf"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "golang.org/x/image"
        #:unpack-path "golang.org/x/image"
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'generate-go-file
            (lambda _
              (call-with-output-file "src/golang.org/x/image/image.go"
                (lambda (port)
                  (format port "package image"))))))))
    (synopsis "x/image")
    (description "x/image")
    (home-page #f)
    (license #f)))

(define-public go-x-oauth2
  (package
    (name "go-x-oauth2")
    (version "v0.0.0-20190220154721-9b3c75971fc9")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "https://go.googlesource.com/oauth2")
          (commit "9b3c75971fc9")))
      (sha256
        (base32
          "1rjzk0bmraszy9n38r5kwrpigfhmnzlna84ak12azz3ydrkwh90y"))))
    (build-system go-build-system)
    (propagated-inputs `(("go-golang-org-x-net" ,go-golang-org-x-net)))
    (arguments
      `(#:import-path "golang.org/x/oauth2"
        #:unpack-path "golang.org/x/oauth2"))
    (synopsis "x/oauth2")
    (description "x/oauth2")
    (home-page #f)
    (license #f)))

(define-public go-imaging
  (package
    (name "go-imaging")
    (version "v1.6.2")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/disintegration/imaging")
          (commit version)))
      (sha256
        (base32
          "1sl201nmk601h0aii4234sycn4v2b0rjxf8yhrnik4yjzd68q9x5"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-x-image" ,go-x-image)))
    (arguments
      `(#:import-path "github.com/disintegration/imaging"
        #:tests? #f))
    (synopsis "imaging")
    (description "imaging")
    (home-page "https://github.com/disintegration/imaging")
    (license #f)))


(define-public go-robotstxt
  (package
    (name "go-robotstxt")
    (version "v1.1.1")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/temoto/robotstxt")
          (commit version)))
      (sha256
        (base32
          "09i59ad7vgyyyf8s40lyyw2kbpn5b34vdnn76zjyxzcj93sn5gw9"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/temoto/robotstxt"
        #:tests? #f))
    (synopsis "robotstxt")
    (description "robotstxt")
    (home-page "https://github.com/temoto/robotstxt")
    (license #f)))


(define-public go-chardet
  (package
    (name "go-chardet")
    (version "v0.0.0-20120816061221-3af4cd4741ca")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/saintfish/chardet")
          (commit "3af4cd4741ca")))
      (sha256
        (base32
          "0czh50md64k9lbllayq0asir3174saxb88yzxrh640yhfxd98pcb"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/saintfish/chardet"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))


(define-public go-htmlquery
  (package
    (name "go-htmlquery")
    (version "v1.0.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/antchfx/htmlquery")
          (commit version)))
      (sha256
        (base32
          "1rsvngqlqyg86g1ff59zmrkn7s8b3c2vpl533amwcv5s2h8npmxy"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-xpath" ,go-xpath)
        ("go-x-net" ,go-x-net)))
    (arguments
      `(#:import-path "github.com/antchfx/htmlquery"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-xpath
  (package
    (name "go-xpath")
    (version "v1.0.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/antchfx/xpath")
          (commit version)))
      (sha256
        (base32
          "0brd75mhd5ix7rz1ijhbggyp53v6g8kz9bc2n7g6zwavzxarfj5p"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/antchfx/xpath"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-glob
  (package
    (name "go-glob")
    (version "v0.2.3")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/gobwas/glob")
          (commit version)))
      (sha256
        (base32
          "0jxk1x806zn5x86342s72dq2qy64ksb3zrvrlgir2avjhwb18n6z"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/gobwas/glob"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-sanitize
  (package
    (name "go-sanitize")
    (version "v1.2.4")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/kennygrant/sanitize")
          (commit version)))
      (sha256
        (base32
          "06f2ljnic3215ihzc5px1q25548ijpixhmn4537gf507n1sxg7iq"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-x-net" ,go-x-net)))
    (arguments
      `(#:import-path "github.com/kennygrant/sanitize"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-xmlquery
  (package
    (name "go-xmlquery")
    (version "v1.0.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/antchfx/xmlquery")
          (commit version)))
      (sha256
        (base32
          "0fc47j4v9q6kjbnmqipqi9cirp30d0kx41bb1mrjmmfkm0jdhky8"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-xpath" ,go-xpath)
        ("go-x-net" ,go-x-net)))
    (arguments
      `(#:import-path "github.com/antchfx/xmlquery"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-colly
  (package
    (name "go-colly")
    (version "v1.2.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/gocolly/colly")
          (commit version)))
      (sha256
        (base32
          "1a6brjy0a4pwq2ml3fvz6p7wjmg37rh006i00zxgv9v4vmv7b84d"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-htmlquery" ,go-htmlquery)
        ("go-goquery" ,go-goquery)
        ("go-xmlquery" ,go-xmlquery)
        ("go-robotstxt" ,go-robotstxt)
        ("go-sanitize", go-sanitize)
        ("go-glob", go-glob)
        ("go-appengine", go-appengine)
        ("go-chardet" ,go-chardet)))
    (arguments
      `(#:import-path "github.com/gocolly/colly"
        #:tests? #f))
    (synopsis "colly")
    (description "colly")
    (home-page "https://github.com/gocolly/colly")
    (license #f)))

(define-public go-appengine
  (package
    (name "go-appengine")
    (version "v1.4.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/golang/appengine")
          (commit version)))
      (sha256
        (base32
          "06zl7w4sxgdq2pl94wy9ncii6h0z3szl4xpqds0sv3b3wbdlhbnn"))))
    (build-system go-build-system)
    (propagated-inputs
      `(("go-protobuf" ,go-protobuf)
        ("go-x-net" ,go-x-net)))
    (arguments
      `(#:import-path "google.golang.org/appengine"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-protobuf
  (package
    (name "go-protobuf")
    (version "v1.2.0")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/golang/protobuf")
          (commit version)))
      (sha256
        (base32
          "0kf4b59rcbb1cchfny2dm9jyznp8ri2hsb14n8iak1q8986xa0ab"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/golang/protobuf"
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'generate-go-file
            (lambda _
              (call-with-output-file "src/github.com/golang/protobuf/protobuf.go"
                (lambda (port)
                  (format port "package protobuf"))))))
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

(define-public go-xxx
  (package
    (name "go-xxx")
    (version "v1.1.1")
    (source
     (origin
      (method git-fetch)
      (file-name (git-file-name name version))
      (uri
        (git-reference
          (url "http://github.com/aaa/xxx")
          (commit version)))
      (sha256
        (base32
          "1sl201nmk601h0aii4234sycn4v2b0rjxf8yhrnik4yjzd68q9x5"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/aaa/xxx"
        #:tests? #f))
    (synopsis "xxx")
    (description "xxx")
    (home-page "https://github.com/aaa/xxx")
    (license #f)))

