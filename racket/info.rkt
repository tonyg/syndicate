#lang setup/infotab
(define collection 'multi)
(define deps '("rfc6455"
	       "base"
	       "data-lib"
	       "net-lib"
               "web-server-lib"
	       "profile-lib"
	       "rackunit-lib"
	       "htdp-lib"
               "data-enumerate-lib"
               "datalog"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "sgl"
               "struct-defaults"
               "auxiliary-macro-context"
               "sandbox-lib"
	       ))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "draw-doc" "gui-doc" "htdp-doc" "pict-doc"))
