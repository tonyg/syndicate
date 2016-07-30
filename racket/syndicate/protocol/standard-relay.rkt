#lang racket/base
;; Relaying of inbound and outbound assertions between adjacent dataspaces.

(provide (struct-out inbound)
         (struct-out outbound)
         inbound*
         outbound*
         spawn-standard-relay)

(require "../trie.rkt")
(require "../relay.rkt")

(struct inbound (assertion) #:prefab)
(struct outbound (assertion) #:prefab)

(define (inbound* n x) (if (zero? n) x (inbound (inbound* (- n 1) x))))
(define (outbound* n x) (if (zero? n) x (outbound (outbound* (- n 1) x))))

(define inbound-parenthesis (open-parenthesis 1 struct:inbound))
(define outbound-parenthesis (open-parenthesis 1 struct:outbound))

(define (spawn-standard-relay inner-spawn)
  (spawn-relay outbound?
               outbound-assertion
               outbound-parenthesis
               inbound
               inbound-parenthesis
               inner-spawn))
