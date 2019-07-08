(define-module (puzzledb-elm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (json)
  #:use-module (elm)
  #:use-module (build-elm)
  #:use-module (versions))

(define* (elm-package package version hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/" package "/archive/" version ".tar.gz"))
    (sha256
      (base32 hash))))

(define elm-sha256
  (elm-package "billstclair/elm-sha256" "1.0.8"
    "0n13iw8hfz9fknfjjw5dd6p5xqb9rksjs3c59dnb0yaa7dag467f"))

(define elm-bytes
  (elm-package "elm/bytes" "1.0.7"
    "040d7irrawcbnq9jxhzx8p9qacdlw5bncy6lgndd6inm53rvvwbp"))

(define elm-browser
  (elm-package "elm/browser" "1.0.1"
    "1zlmx672glg7fdgkvh5jm47y85pv7pdfr5mkhg6x7ar6k000vyka"))

(define elm-list-extra
  (elm-package "elm-community/list-extra" "8.1.0"
    "0ngynq305b45zkz0398z66ldb1xl3jjnzdrj8fddim1zxjv2jdiw"))

(define elm-core
  (elm-package "elm/core" "1.0.2"
    "1l0qdbczw91kzz8sx5d5zwz9x662bspy7p21dsr3f2rigxiix2as"))

(define elm-markdown
  (elm-package "elm-explorations/markdown" "1.0.0"
    "0k3110ixa4wwf3vkkdplagwah9ypr965qxr1y147rnsc1xsxmr6y"))

(define elm-file
  (elm-package "elm/file" "1.0.1"
    "15vw1ilbg0msimq2k9magwigp8lwqrgvz3vk6qia6b3ldahvw8jr"))

(define elm-html
  (elm-package "elm/html" "1.0.0"
    "1n3gpzmpqqdsldys4ipgyl1zacn0kbpc3g4v3hdpiyfjlgh8bf3k"))

(define elm-http
  (elm-package "elm/http" "1.0.0"
    "1igmm89ialzrjib1j8xagkxalq1x2gj4l0hfxcd66mpwmvg7psl8"))

(define elm-json
  (elm-package "elm/json" "1.1.2"
    "1a107nmm905dih4w4mjjkkpdcjbgaf5qjvr7fl30kkpkckfjjnrw"))

(define elm-parser
  (elm-package "elm/parser" "1.1.0"
    "0a3cxrvbm7mwg9ykynhp7vjid58zsw03r63qxipxp3z09qks7512"))

(define elm-random
  (elm-package "elm/random" "1.0.0"
    "138n2455wdjwa657w6sjq18wx2r0k60ibpc4frhbqr50sncxrfdl"))

(define elm-regex
  (elm-package "elm/regex" "1.0.0"
    "0lijsp50w7n1n57mjg6clpn9phly8vvs07h0qh2rqcs0f1jqvsa2"))

(define elm-svg
  (elm-package "elm/svg" "1.0.1"
    "1cwcj73p61q45wqwgqvrvz3aypjyy3fw732xyxdyj6s256hwkn0k"))

(define elm-time
  (elm-package "elm/time" "1.0.0"
    "0vch7i86vn0x8b850w1p69vplll1bnbkp8s383z7pinyg94cm2z1"))

(define elm-url
  (elm-package "elm/url" "1.0.0"
    "0av8x5syid40sgpl5vd7pry2rq0q4pga28b4yykn9gd9v12rs3l4"))

(define elm-virtual-dom
  (elm-package "elm/virtual-dom" "1.0.2"
    "0q1v5gi4g336bzz1lgwpn5b1639lrn63d8y6k6pimcyismp2i1yg"))

(define elm-html-parser
  (elm-package "hecrj/html-parser" "2.2.0"
    "06dl1lgwf0c2z8lm8w443qbvldidx0dj0q8r5j6308mrj7blsm9d"))

(define elm-pointer-events
  (elm-package "mpizenberg/elm-pointer-events" "4.0.0"
    "1wh0xvs17c43wjmilv6hysqnjk8hqnjp69dhjvqpy9fa92hisl5k"))

(define elm-hex
  (elm-package "rtfeldman/elm-hex" "1.0.0"
    "1y0aa16asvwdqmgbskh5iba6psp43lkcjjw9mgzj3gsrg33lp00d"))

(define elm-color
  (elm-package "the-sett/elm-color" "1.0.1"
    "0v5pir6pvbg8z98li8z66pv7rz8nv00b6zbic6ly6v9q8fqwx8da"))

(define elm-collage
  (elm-package "timjs/elm-collage" "2.0.0"
    "1nwdvvkm7jgy0z9zw72x4kvadg6mjf79fkj2cbwdq2lh4zdqwa4y"))

(define-public puzzledb-elm
 (package
   (name "puzzledb-elm")
   (version "20190625-git")
   (source
      (local-file
        "/home/rob/puzzledb/frontend"
        #:recursive? #t))
   (build-system trivial-build-system)
   (native-inputs
    `(("elm-compiler" ,elm-compiler)
      ("tar" ,tar)
      ("gzip" ,gzip)
      ("elm-sha256" ,elm-sha256)
      ("elm-bytes" ,elm-bytes)
      ("elm-browser" ,elm-browser)
      ("elm-list-extra" ,elm-list-extra)
      ("elm-core" ,elm-core)
      ("elm-markdown" ,elm-markdown)
      ("elm-file" ,elm-file)
      ("elm-html" ,elm-html)
      ("elm-http" ,elm-http)
      ("elm-json" ,elm-json)
      ("elm-parser" ,elm-parser)
      ("elm-random" ,elm-random)
      ("elm-regex" ,elm-regex)
      ("elm-svg" ,elm-svg)
      ("elm-time" ,elm-time)
      ("elm-url" ,elm-url)
      ("elm-virtual-dom" ,elm-virtual-dom)
      ("elm-html-parser" ,elm-html-parser)
      ("elm-pointer-events" ,elm-pointer-events)
      ("elm-hex" ,elm-hex)
      ("elm-color" ,elm-color)
      ("elm-collage" ,elm-collage)))
    (arguments
     `(#:modules ((guix build utils) (build-elm) (json parser) (versions))
       #:builder
         (begin
           (use-modules (guix build utils) (ice-9 match) (build-elm) (json parser) (versions))
           (let* ((elmpkg (assoc-ref %build-inputs "elm-compiler"))
                  (elm (string-append elmpkg "/bin/elm"))
                  (tar (assoc-ref %build-inputs "tar"))
                  (gzip (assoc-ref %build-inputs "gzip"))
                  (deps ".elm/0.19.0/package")
                  (out (assoc-ref %outputs "out")))
             (setenv "PATH" (string-append (getenv "PATH") ":" (string-append tar "/bin") ":" (string-append gzip "/bin")))
             (format #t "unpacking dependencies~%")
             (mkdir-p deps)
             (for-each
               (match-lambda
                 ((n . pkg)
                  (when (and (string-prefix? "elm-" n)
                             (not (equal? "elm-compiler" n)))
                    (elm-unpack pkg deps))))
               %build-inputs)
             (format #t "building versions.dat~%")
             (with-directory-excursion deps (build-versions.dat))
             (format #t "setting up elm env: cwd=~a~%" (getcwd))
             (setenv "HOME" (getcwd))
             (setenv "HTTP_PROXY" ".")
             (system "echo $HOME/.elm/*/*/*")
             (copy-recursively (assoc-ref %build-inputs "source") "src")
             (with-directory-excursion "src"
               (format #t "compiling elm source code~%")
               (mkdir "elm-stuff")
               (chmod "elm-stuff" #o775)
               (for-each
                 (match-lambda
                   ((src . dst)
                    (invoke elm "make"
                      (string-append "--output=" out "/" dst)
                      (string-append "src/" src))))
                 '(("All.elm" . "all.js")
                   ("Puzzle.elm" . "puzzle.js")
                   ("Blogs.elm" . "blogs.js")))))
           #t)))
    (description #f)
    (synopsis #f)
    (license #f)
    (home-page #f)))

(define-public puzzledb-frontend
 (package
   (name "puzzledb-frontend")
   (version "20190625-git")
   (source
      (local-file
        "/home/rob/puzzledb/frontend"
        #:recursive? #t))
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
              '("style.css" "all.html" "blogs.html" "puzzle.html"))
            (symlink "all.html" (string-append out "/index.html"))))))
   (description #f)
   (synopsis #f)
   (license #f)
   (home-page #f)))
