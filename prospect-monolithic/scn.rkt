#lang racket/base
;; State Change Notifications, and assorted protocol constructors

(provide (struct-out scn)
         (struct-out observe)
         (struct-out at-meta)
         (struct-out advertise)
         lift-scn
         drop-scn
         strip-interests
         label-interests
         strip-scn
         label-scn
         biased-intersection)

(require racket/set)
(require racket/match)
(require "../prospect/route.rkt")
(require "../prospect/tset.rkt")
(require "../prospect/pretty.rkt")
(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; State Change Notifications
(struct scn (trie) #:transparent
  #:methods gen:prospect-pretty-printable
  [(define (prospect-pretty-print d [p (current-output-port)])
     (pretty-print-trie (scn-trie d) p))])

;; Claims, Interests, Locations, and Advertisements
(struct observe (claim) #:prefab)
(struct at-meta (claim) #:prefab)
(struct advertise (claim) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define at-meta-proj (compile-projection (at-meta (?!))))

(define (lift-scn s)
  (scn (pattern->trie #t (at-meta (embedded-trie (scn-trie s))))))

(define (drop-interests pi)
  (trie-project pi at-meta-proj
                   #:project-success (lambda (v) #t)
                   #:combiner (lambda (v1 v2) #t)))

(define (drop-scn s)
  (scn (drop-interests (scn-trie s))))

(define (strip-interests g)
  (trie-relabel g (lambda (v) #t)))

(define (label-interests g label)
  (trie-relabel g (lambda (v) label)))

(define (strip-scn s)
  (scn (strip-interests (scn-trie s))))

(define (label-scn s label)
  (scn (label-interests (scn-trie s) label)))

(define (biased-intersection object subject)
  (trie-intersect object
		  (trie-step subject struct:observe)
		  #:combiner (lambda (v1 v2) #t)
		  #:left-short (lambda (v r) (trie-step r EOS))))
