#lang racket/base
;; Generic protocol for WebSockets/TCP/etc-based participation in a network.

(provide drop-json-action
	 lift-json-event
	 lift-json-action
	 drop-json-event
	 ping-interval)

(require net/rfc6455)
(require racket/set)
(require racket/match)
(require "../main.rkt")
(require "../tset.rkt")
(require "../support/struct.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wire protocol representation of events and actions

(define (revive-prefabs j)
  (match j
    [(? list?) (map revive-prefabs j)]
    [(? hash?)
     (define type (hash-ref j '@type #f))
     (define fields (hash-ref j 'fields #f))
     (if (and type fields (= (hash-count j) 2))
         (apply (struct-type-make-constructor
                 (prefab-key->struct-type (string->symbol type) (length fields)))
                (map revive-prefabs fields))
         (for/hasheq [((k v) (in-hash j))] (values k (revive-prefabs v))))]
    [_ j]))

(define (pickle-prefabs j)
  (match j
    [(? list?) (map pickle-prefabs j)]
    [(? hash?) (for/hasheq [((k v) (in-hash j))] (values k (pickle-prefabs v)))]
    [(? non-object-struct?)
     (hasheq '@type (symbol->string (struct-type-name (struct->struct-type j)))
             'fields (map pickle-prefabs (cdr (vector->list (struct->vector j)))))]
    [_ j]))

(define only-peer (datum-tset 'peer))

(define (drop j)
  (match (revive-prefabs j)
    ["ping" 'ping]
    ["pong" 'pong]
    [`("patch" ,pj) (jsexpr->patch pj
                                   (lambda (v) only-peer)
                                   (lambda (arity t)
                                     (prefab-key->struct-type (string->symbol t) arity)))]
    [`("message" ,body) (message body)]))

(define (lift j)
  (pickle-prefabs
   (match j
     ['ping "ping"]
     ['pong "pong"]
     [(? patch? p) `("patch" ,(patch->jsexpr p (lambda (v) #t)))]
     [(message body) `("message" ,body)])))

(define drop-json-action drop)
(define lift-json-event lift)
(define lift-json-action lift)
(define drop-json-event drop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections

(define (ping-interval) (* 1000 (max (- (ws-idle-timeout) 10) (* (ws-idle-timeout) 0.8))))
