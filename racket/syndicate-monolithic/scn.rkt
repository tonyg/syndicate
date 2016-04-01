#lang racket/base
;; State Change Notifications, and assorted protocol constructors

(provide (struct-out scn)
         (struct-out observe)
         (struct-out at-meta)
         (struct-out advertise)
         observe-parenthesis
         at-meta-parenthesis
         lift-scn
         drop-scn
         strip-interests
         label-interests
         strip-scn
         label-scn
         biased-intersection)

(require racket/set)
(require racket/match)
(require "../syndicate/trie.rkt")
(require "../syndicate/tset.rkt")
(require "../syndicate/pretty.rkt")
(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; State Change Notifications
(struct scn (trie) #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print d [p (current-output-port)])
     (pretty-print-trie (scn-trie d) p))])

;; Claims, Interests, Locations, and Advertisements
(struct observe (claim) #:prefab)
(struct at-meta (claim) #:prefab)
(struct advertise (claim) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define observe-parenthesis (open-parenthesis 1 struct:observe))
(define at-meta-parenthesis (open-parenthesis 1 struct:at-meta))

(define (lift-scn s)
  (scn (pattern->trie '<lift-scn> (at-meta (embedded-trie (scn-trie s))))))

(define (drop-interests pi)
  (trie-step pi at-meta-parenthesis))

(define (drop-scn s)
  (scn (drop-interests (scn-trie s))))

(define (strip-interests g)
  (trie-relabel g (lambda (v) '<strip-interests>)))

(define (label-interests g label)
  (trie-relabel g (lambda (v) label)))

(define (strip-scn s)
  (scn (strip-interests (scn-trie s))))

(define (label-scn s label)
  (scn (label-interests (scn-trie s) label)))

(define (biased-intersection object subject)
  (trie-intersect object
		  (trie-step subject observe-parenthesis)
		  #:combiner (lambda (v1 v2) (trie-success v1))))
