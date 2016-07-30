#lang racket/base
;; Advertisement assertions.

(provide (struct-out advertise)
         advertisement
         pub
         unpub)

(require racket/match)
(require "../core.rkt")
(require "../trie.rkt")
(require "standard-relay.rkt")

(struct advertise (claim) #:prefab)

(define (drop-advertisement a)
  (match a
    [(advertise (inbound x)) (outbound (advertise x))]
    [_ a]))

;; Monolithic SCN
(define (advertisement pattern)
  (pattern->trie '<advertisement> (drop-advertisement (advertise pattern))))

;; Incremental SCNs
(define (pub pattern) (assert (drop-advertisement (advertise pattern))))
(define (unpub pattern) (retract (drop-advertisement (advertise pattern))))
