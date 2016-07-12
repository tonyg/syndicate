#lang racket/base
;; State Change Notifications, and assorted protocol constructors

(provide (struct-out scn)
         lift-scn
         drop-scn
         strip-scn
         label-scn)

(require racket/set)
(require racket/match)
(require "../trie.rkt")
(require "../patch.rkt")
(require "../pretty.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; State Change Notifications
(struct scn (trie) #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print d [p (current-output-port)])
     (pretty-print-trie (scn-trie d) p))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lift-scn s)
  (scn (pattern->trie '<lift-scn> (at-meta (embedded-trie (scn-trie s))))))

(define (drop-scn s)
  (scn (drop-interests (scn-trie s))))

(define (strip-scn s)
  (scn (strip-interests (scn-trie s))))

(define (label-scn s label)
  (scn (label-interests (scn-trie s) label)))
