#lang racket

(provide (struct-out upside-down)
         upside-down-parenthesis
         spawn-upside-down
         upside-down-relay)

(require "core.rkt")
(require "trie.rkt")
(require "protocol/standard-relay.rkt")

#|
A module that takes actors to ... the upside down [1]. Actors in the upside down
may communicate with each other, but not the outside world. However, the outside
world can see what actors in the upside-down are saying. The intention is to use
this facility for testing.

[1] (http://strangerthings.wikia.com/wiki/Upside_Down).
|#

(struct upside-down (v) #:transparent)
(define upside-down-parenthesis (open-parenthesis 1 struct:upside-down))

;; during assertions (upside-down (outbound X)) assert (outbound X)
;; on messages (message (upside-down (outbound X))) send (message (outbound X))
;; during assertions (upside-down (observe (inbound X))) assert
;; (observe (inbound X))
(define upside-down-relay
  (actor (lambda (e s)
           (define (upside-down-relay-trie t)
             (define outbounds (trie-project t (?! (outbound ?))))
             (define subscriptions
               (trie-project t (upside-down (?! (observe (inbound ?))))))
             (trie-union outbounds subscriptions))
           (match e
             [(message (upside-down (outbound x)))
              (transition #f (list (message (outbound x))))]
             [(patch added removed)
              (transition #f (list (patch (upside-down-relay-trie added)
                                          (upside-down-relay-trie removed))))]
             [_ #f]))
         #f
         (list (patch-seq (sub (upside-down (outbound ?)))
                          (sub (upside-down (observe (inbound ?))))))))

(define (spawn-upside-down inner-spawn)
  (make-actor (lambda ()
                (define-values (proc initial-transition)
                  (boot->process+transition (actor-boot inner-spawn)))
                (list (upside-down-behavior (process-behavior proc))
                      (upside-down-transition initial-transition)
                      (process-name proc)))
              (actor-initial-assertions inner-spawn)))

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
    [(? actor? a) (spawn-upside-down a)]))

;; Event -> Event
(define (turn-event-rightside-up e)
  (match e
    [#f #f]
    [(patch added removed)
     (patch (turn-trie-rightside-up added)
            (turn-trie-rightside-up removed))]
    [(message (upside-down v))
     (message v)]
    [(message (inbound v))
     (message (inbound v))]))

;; Trie -> Trie
;; x ∈ T => (upside-down x) ∈ T'
;; ?x ∈ T => ?(upside-down x) ∈ T'
;; (outbound x) ∈ T => (outbound x) ∈ T'
;; (observe (inbound x)) ∈ T => (observe (inbound x)) ∈ T'
(define (turn-trie-upside-down t)
  (define subscriptions (trie-project t (observe (?!))))
  (define outgoing (trie-project t (?! (outbound ?))))
  (define inbound-interest (trie-project t (?! (observe (inbound ?)))))
  (trie-union-all (list (trie-prepend upside-down-parenthesis t)
                        (trie-prepend observe-parenthesis
                                      (trie-prepend upside-down-parenthesis
                                                    subscriptions))
                        outgoing
                        inbound-interest)))

(define (turn-trie-rightside-up t)
  (define upside-downs (trie-project t (upside-down (?!))))
  (define inbounds (trie-project t (?! (inbound ?))))
  (trie-union upside-downs inbounds))
