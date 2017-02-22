#lang racket

(provide (struct-out upside-down)
         upside-down-parenthesis
         spawn-upside-down)

(require "core.rkt")
(require "trie.rkt")

#|
A module that takes actors to ... the upside down [1]. Actors in the upside down
may communicate with each other, but not the outside world. However, the outside
world can see what actors in the upside-down are saying. The intention is to use
this facility for testing.

[1] (http://strangerthings.wikia.com/wiki/Upside_Down).
|#

(struct upside-down (v) #:transparent)
(define upside-down-parenthesis (open-parenthesis 1 struct:upside-down))

(define (spawn-upside-down inner-spawn)
  (make-actor (lambda ()
                (define-values (proc initial-transition)
                  (actor->process+transition inner-spawn))
                (list (upside-down-behavior (process-behavior proc))
                      (upside-down-transition initial-transition)
                      (process-name proc)))))

;; Transition -> Transition
(define (upside-down-transition t)
  (match t
    [(<quit> exn actions)
     (<quit> exn (upside-down-actions actions))]
    [(transition st actions)
     (transition st (upside-down-actions actions))]
    [(or #f (? void?))
     t]))

(define ((upside-down-behavior b) e s)
  (define rightside-up-e
    (turn-event-rightside-up e))
  (upside-down-transition (b rightside-up-e s)))

(define (upside-down-actions acs)
  (filter-map turn-action-upside-down (clean-actions acs)))

;; Action -> Action
(define (turn-action-upside-down a)
  (match a
    [#f #f]
    [(message v)
     (message (upside-down v))]
    [(patch added removed)
     (patch (turn-trie-upside-down added)
            (turn-trie-upside-down removed))]
    [(? actor? a) a]))

;; Event -> Event
(define (turn-event-rightside-up e)
  (match e
    [#f #f]
    [(patch added removed)
     (patch (trie-project added (upside-down (?!)))
            (trie-project removed (upside-down (?!))))]
    [(message (upside-down v))
     (message v)]))

;; Trie -> Trie
;; x ∈ T => (upside-down x) ∈ T'
;; ?x ∈ T => ?(upside-down x) ∈ T'
(define (turn-trie-upside-down t)
  (define subscriptions (trie-project t (observe (?!))))
  (trie-union (trie-prepend upside-down-parenthesis t)
              (trie-prepend observe-parenthesis
                            (trie-prepend upside-down-parenthesis
                                          subscriptions))))