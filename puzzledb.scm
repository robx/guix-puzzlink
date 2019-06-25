(define-module (puzzledb)
  #:use-module (gnu packages golang)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go))

(define-public puzzledb-tools
  (package
    (name "puzzledb-tools")
    (version "20190625-git")
    (source
      (local-file
        "/home/rob/puzzledb/tools"
        #:recursive? #t))
;      (origin
;        (method git-fetch)
;        (file-name (git-file-name name version))
;        (uri
;          (git-reference
;            (url "/home/rob/puzzledb")
;            (commit "master")))
;        (sha256
;          (base32
;            "06g65r3z52layafrx50nipz4by6mjjbg6arp238fazqd3rzlbm79"))))
;    (build-system go-build-system)
;    (arguments
;      `(#:import-path "src"))
;    (build-system gnu-build-system)
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
        ("go-x-net-html" ,go-x-net-html)
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
      `(("go-x-net-html" ,go-x-net-html)))
    (arguments
      `(#:import-path "github.com/andybalholm/cascadia"))
    (synopsis "gocascadia")
    (description "gocascadia")
    (home-page "https://github.com/andybalholm/cascadiia")
    (license #f)))

(define-public go-x-net-html
  (package
    (name "go-x-net-html")
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
;      `(("go-golang-org-x-text-transform" ,go-golang-org-x-text-transform)
;        ("go-x-text-encoding" ,go-x-text-encoding)))
    (arguments
      `(#:import-path "golang.org/x/net/html"
        #:unpack-path "golang.org/x/net"))
    (synopsis "x/net/html")
    (description "x/net/html")
    (home-page #f)
    (license #f)))

;(define-public go-x-text-encoding
(define-public go-x-text
  (package
;    (name "go-x-text-encoding")
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
;      `(#:import-path "golang.org/x/text/encoding"
      `(#:import-path "golang.org/x/text"
        #:unpack-path "golang.org/x/text"))
    (synopsis "x/text/encoding")
    (description "x/text/encoding")
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
    (propagated-inputs `(("go-golang-org-x-net-context" ,go-golang-org-x-net-context)))
    (arguments
      `(#:import-path "golang.org/x/oauth2"
        #:unpack-path "golang.org/x/oauth2"))
    (synopsis "x/oauth2")
    (description "x/oauth2")
    (home-page #f)
    (license #f)))
     
