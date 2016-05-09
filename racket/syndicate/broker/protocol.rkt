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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wire protocol representation of events and actions

(define only-peer (datum-tset 'peer))

(define (drop j)
  (match j
    ["ping" 'ping]
    ["pong" 'pong]
    [`("patch" ,pj) (jsexpr->patch pj (lambda (v) only-peer))]
    [`("message" ,body) (message body)]))

(define (lift j)
  (match j
    ['ping "ping"]
    ['pong "pong"]
    [(? patch? p) `("patch" ,(patch->jsexpr p (lambda (v) #t)))]
    [(message body) `("message" ,body)]))

(define drop-json-action drop)
(define lift-json-event lift)
(define lift-json-action lift)
(define drop-json-event drop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections

(define (ping-interval) (* 1000 (max (- (ws-idle-timeout) 10) (* (ws-idle-timeout) 0.8))))
