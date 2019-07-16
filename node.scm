(define-module (node)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system node))

(define-public coffeescript
  (package
    (name "coffeescript")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/coffeescript/-/coffeescript-2.4.1.tgz")
       (sha256
        (base32
         "0wc4bb12mjvsnszncavc6ig9npm2z5xhl25hars2rpss31bzkw74"))))
    (build-system node-build-system)    
    (arguments `(#:tests? #f)) ; tests not included
    (synopsis "CoffeeScript is a little language that compiles into JavaScript")
    (description "CoffeeScript is a little language that compiles into JavaScript.
Underneath that awkward Java-esque patina, JavaScript has always had a gorgeous heart.
CoffeeScript is an attempt to expose the good parts of JavaScript in a simple way.

The golden rule of CoffeeScript is: “It’s just JavaScript.” The code compiles
one-to-one into the equivalent JS, and there is no interpretation at runtime.
You can use any existing JavaScript library seamlessly from CoffeeScript
(and vice-versa).  The compiled output is readable, pretty-printed, and tends
to run as fast or faster than the equivalent handwritten JavaScript.")
    (home-page "https://coffeescript.org/")
    (license license:expat)))
