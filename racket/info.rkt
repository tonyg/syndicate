#lang setup/infotab
(define collection 'multi)
(define deps '(
	       "base"
	       "data-lib"
	       "htdp-lib"
	       "net-lib"
	       "profile-lib"
	       "rackunit-lib"
	       "sha"
               "automata"
               "auxiliary-macro-context"
               "data-enumerate-lib"
               "datalog"
               "db-lib"
               "draw-lib"
               "gui-lib"
               "images-lib"
               "macrotypes-lib"
               "pict-lib"
               "rackunit-macrotypes-lib"
               "rfc6455"
               "sandbox-lib"
               "sgl"
               "struct-defaults"
               "turnstile-example"
               "turnstile-lib"
               "web-server-lib"
               ))
(define build-deps '(
                     "draw-doc"
                     "gui-doc"
                     "htdp-doc"
                     "pict-doc"
                     "racket-doc"
                     "scribble-lib"
                     "sha"
                     ))

(define test-omit-paths
  ;; There's some shared library related build issue with the syndicate-gl things
  '("syndicate-gl/"
    "syndicate-ide/"))
