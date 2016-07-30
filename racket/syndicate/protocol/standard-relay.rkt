#lang racket/base
;; Relaying of inbound and outbound assertions between adjacent dataspaces.

(provide (struct-out inbound)
         (struct-out outbound)
         inbound*
         outbound*
         inbound**
         outbound**
         uninbound**
         unoutbound**
         spawn-standard-relay)

(require racket/match)
(require "../trie.rkt")
(require "../relay.rkt")

(struct inbound (assertion) #:prefab)
(struct outbound (assertion) #:prefab)

(define (inbound** n x) (if (zero? n) x (inbound (inbound** (- n 1) x))))
(define (outbound** n x) (if (zero? n) x (outbound (outbound** (- n 1) x))))

(define (uninbound** n x) (cond [(zero? n) (values #t x)]
                                [(inbound? x) (uninbound** (- n 1) (inbound-assertion x))]
                                [else (values #f #f)]))
(define (unoutbound** n x) (cond [(zero? n) (values #t x)]
                                 [(outbound? x) (unoutbound** (- n 1) (outbound-assertion x))]
                                 [else (values #f #f)]))

(define-match-expander inbound*
  (syntax-rules () [(_ n x) (app (lambda (v) (uninbound** n v)) #t x)])
  (syntax-rules () [(_ n x) (inbound** n x)]))
(define-match-expander outbound*
  (syntax-rules () [(_ n x) (app (lambda (v) (unoutbound** n v)) #t x)])
  (syntax-rules () [(_ n x) (outbound** n x)]))

(define inbound-parenthesis (open-parenthesis 1 struct:inbound))
(define outbound-parenthesis (open-parenthesis 1 struct:outbound))

(define (spawn-standard-relay inner-spawn)
  (spawn-relay outbound?
               outbound-assertion
               outbound-parenthesis
               inbound
               inbound-parenthesis
               inner-spawn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (define (i* v)
    (match v
      [(inbound* 0 'hi) 'hi0]
      [(inbound* 1 'hi) 'hi1]
      [(inbound* 2 'there) 'there2]
      [(inbound* 2 'hi) 'hi2]
      [(inbound* 3 'hi) 'hi3]
      [other (list 'other other)]))

  (check-equal? (i* (inbound* 2 'hi)) 'hi2)
  (check-equal? (i* (inbound* 2 'there)) 'there2)
  (check-equal? (i* 'hi) 'hi0)
  (check-equal? (i* 'there) (list 'other 'there)))
