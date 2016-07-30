#lang racket/base
;; Rewrite assertions at a boundary between a containing dataspace and
;; a contained actor (or dataspace).

(provide (struct-out relay)
         spawn-relay
         relay-handle-event
         pretty-print-relay)

(require racket/match)
(require (only-in racket/list filter-map))
(require "core.rkt")
(require "trie.rkt")
(require "pretty.rkt")
(require "hierarchy.rkt")

(struct relay (outbound? ;; Assertion -> Boolean
               outbound-assertion ;; Assertion -> Assertion
               outbound-parenthesis ;; OpenParenthesis/1
               inbound-constructor ;; Assertion -> Assertion
               inbound-parenthesis ;; OpenParenthesis/1
               inner-behavior ;; Behavior
               inner-state ;; Any
               )
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print r [p (current-output-port)])
     (pretty-print-relay r p))])

(define (relay-lift-event e r)
  (match e
    [#f #f]
    [(? targeted-event?) e]
    [(message c) (message ((relay-inbound-constructor r) c))]
    [(patch a d) (patch (trie-prepend (relay-inbound-parenthesis r) a)
                        (trie-prepend (relay-inbound-parenthesis r) d))]))

(define (relay-drop-interests t r)
  (define interesting-inbound-assertions
    (trie-step (trie-step t observe-parenthesis) (relay-inbound-parenthesis r)))
  (define ordinary-outbound-assertions
    (trie-step t (relay-outbound-parenthesis r)))
  (define additional-outbound-assertions-of-interest
    (trie-prepend observe-parenthesis interesting-inbound-assertions))
  (trie-union ordinary-outbound-assertions
              additional-outbound-assertions-of-interest))

(define (relay-drop-action ac r)
  (match ac
    [(message c)
     (and ((relay-outbound? r) c)
          (message ((relay-outbound-assertion r) c)))]
    [(patch a d)
     (define p (patch (relay-drop-interests a r) (relay-drop-interests d r)))
     (and (patch-non-empty? p) p)]
    [_
     ;; TODO: What should be done about spawn? Anything?
     ;; TODO: How about quit-dataspace? Could this be a better place for it than core.rkt?
     (error 'relay-drop-action "Cannot drop action ~v" ac)]))

(define (relay-drop-actions acs r)
  (filter-map (lambda (ac) (relay-drop-action ac r)) (clean-actions acs)))

(define (relay-transition t r)
  (match t
    [(<quit> exn actions)
     (<quit> exn (relay-drop-actions actions r))]
    [(transition st actions)
     (transition (struct-copy relay r [inner-state st]) (relay-drop-actions actions r))]
    [(or #f (? void?))
     t]))

(define (relay-handle-event e r)
  (relay-transition ((relay-inner-behavior r) (relay-lift-event e r) (relay-inner-state r)) r))

(define ((inject-relay-subscription r) initial-inner-state)
  (define initial-patch
    (patch-seq (patch (trie-prepend observe-parenthesis
                                    (trie-prepend (relay-outbound-parenthesis r)
                                                  (pattern->trie '<relay> ?)))
                      trie-empty)
               (sub (observe ((relay-inbound-constructor r) ?)))))
  ((relay-inner-behavior r) initial-patch initial-inner-state))

(define (spawn-relay outbound?
                     outbound-assertion
                     outbound-parenthesis
                     inbound-constructor
                     inbound-parenthesis
                     inner-spawn)
  (<spawn> (lambda ()
             (match-define (list inner-behavior initial-transition name) ((spawn-boot inner-spawn)))
             (define initial-relay-state (relay outbound?
                                                outbound-assertion
                                                outbound-parenthesis
                                                inbound-constructor
                                                inbound-parenthesis
                                                inner-behavior
                                                'uninitialized:initial-inner-state))
             (list relay-handle-event
                   (relay-transition (transition-bind (inject-relay-subscription initial-relay-state)
                                                      initial-transition)
                                     initial-relay-state)
                   name))))

(define (pretty-print-relay r p)
  (fprintf p "RELAY ~a/~a\n"
           (open-parenthesis-type (relay-outbound-parenthesis r))
           (open-parenthesis-type (relay-inbound-parenthesis r)))
  (syndicate-pretty-print (relay-inner-state r) p))
