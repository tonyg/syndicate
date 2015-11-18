#lang racket/base
;; Marketplace-style endpoints (analogous to threads)

(provide (struct-out endpoint-group)
         (struct-out add-endpoint)
         (struct-out delete-endpoint)
         make-endpoint-group
         spawn-endpoint-group
         endpoint-action?
         (struct-out endpoint)
         endpoint-group-handle-event
         pretty-print-endpoint-group)

(require racket/set)
(require racket/match)
(require (only-in racket/list flatten))
(require "route.rkt")
(require "patch.rkt")
(require "core.rkt")
(require "mux.rkt")
(require "pretty.rkt")
(require "tset.rkt")

;; An EID is a Nat.

;; Endpoint-group private states
(struct endpoint-group (next-eid ;; EID
                        routing-table ;; (Matcherof (Setof EID))
                        interests ;; (HashTable EID Matcher)
                        endpoints ;; (HashTable EID Endpoint)
                        state ;; Any
                        )
  #:transparent
  #:methods gen:prospect-pretty-printable
  [(define (prospect-pretty-print g [p (current-output-port)])
     (pretty-print-endpoint-group g p))])

;; A Handler is a (Event State -> Transition)
;; A Transition reuses the struct from core, but with EndpointActions instead of plain Actions.
;; An EndpointAction is either an Action, or a
;; (add-endpoint (EID State -> (Values Endpoint Transition))), or a
;; (delete-endpoint EID)
(struct add-endpoint (function) #:prefab)
(struct delete-endpoint (eid) #:prefab)

(define (make-endpoint-group initial-state)
  (endpoint-group 0
                  (matcher-empty)
                  (hash)
                  (hash)
                  initial-state))

(define-syntax-rule (spawn-endpoint-group initial-state add-endpoint-instruction ...)
  (<spawn> (lambda ()
             (define-values (final-cumulative-patch final-actions final-g)
               (interpret-endpoint-actions empty-patch
                                           '()
                                           (make-endpoint-group initial-state)
                                           -1
                                           (list add-endpoint-instruction ...)))
             (when (not (null? final-actions))
               (error 'spawn-endpoint-group
                      "Unexpected initial actions: ~v" final-actions))
             (list final-cumulative-patch
                   endpoint-group-handle-event
                   final-g))))

(define (endpoint-action? a)
  (or (action? a)
      (add-endpoint? a)
      (delete-endpoint? a)))

;; An Endpoint represents the behaviour of an endpoint.
(struct endpoint (pre-handler ;; Handler
                  peri-handler ;; Handler
                  post-handler ;; Handler
                  ) #:transparent)

(define (inert-handler e state) #f)

(define inert-endpoint
  (endpoint inert-handler
            inert-handler
            inert-handler))

(define (endpoint-group-handle-event e g)
  (match-define (endpoint-group _ routing-table interests endpoints state) g)
  (define affected-eids
    (match e
      [#f (hash-keys endpoints)]
      [(? patch?) (compute-affected-pids routing-table e)]
      [(message body)
       (tset->list (matcher-match-value routing-table (observe body) (datum-tset)))]))
  (define tasks (for/list [(eid affected-eids)]
                  (list (if (patch? e)
                            (view-patch e (hash-ref interests eid matcher-empty))
                            e)
                        eid
                        (hash-ref endpoints eid inert-endpoint))))
  (define t0 (transition g '()))
  (define t1 (sequence-transitions t0
                                   (sequence-handlers tasks endpoint-pre-handler)
                                   (sequence-handlers tasks endpoint-peri-handler)
                                   (sequence-handlers tasks endpoint-post-handler)))
  (if (eq? t1 t0) #f t1))

(define ((sequence-handlers tasks handler-getter) g)
  (let/ec return
    (define-values (final-cumulative-patch final-actions final-g idle?)
      (for/fold ([cumulative-patch empty-patch]
                 [actions '()]
                 [g g]
                 [idle? #t])
                ([task tasks])
        (match-define (list e eid ep) task)
        (match ((handler-getter ep) e (endpoint-group-state g))
          [#f (values cumulative-patch actions g idle?)]
          [(<quit> exn ep-acs) (return (<quit> exn (filter action? (flatten ep-acs))))]
          [(transition new-state ep-acs)
           (define-values (cp acs next-g)
             (interpret-endpoint-actions cumulative-patch
                                         actions
                                         (struct-copy endpoint-group g [state new-state])
                                         eid
                                         ep-acs))
           (values cp acs next-g #f)])))
    (if idle?
        #f
        (transition final-g (incorporate-cumulative-patch final-actions final-cumulative-patch)))))

(define (incorporate-cumulative-patch actions cumulative-patch)
  (if (patch-empty? cumulative-patch)
      actions
      (cons actions cumulative-patch)))

(define (interpret-endpoint-patch cumulative-patch actions g eid p0)
  (define old-interests (hash-ref (endpoint-group-interests g) eid matcher-empty))
  (define old-routing-table (endpoint-group-routing-table g))
  (define p (limit-patch (label-patch p0 (datum-tset eid)) old-interests))
  (define p-aggregate (compute-aggregate-patch p eid old-routing-table))
  (define new-interests (apply-patch old-interests p))
  (define new-routing-table (apply-patch old-routing-table p))
  (values (patch-seq cumulative-patch p-aggregate)
          actions
          (struct-copy endpoint-group g
                       [routing-table new-routing-table]
                       [interests (if (matcher-empty? new-interests)
                                      (hash-remove (endpoint-group-interests g) eid)
                                      (hash-set (endpoint-group-interests g)
                                                eid
                                                new-interests))])))

(define (interpret-endpoint-action cumulative-patch actions g eid endpoint-action)
  (match endpoint-action
    [(or (? message?)
         (? spawn?))
     (values empty-patch
             (cons (incorporate-cumulative-patch actions cumulative-patch) endpoint-action)
             g)]
    [(? patch? p0)
     (interpret-endpoint-patch cumulative-patch actions g eid p0)]
    [(add-endpoint function)
     (define new-eid (endpoint-group-next-eid g))
     (define-values (new-ep initial-transition) (function new-eid (endpoint-group-state g)))
     (interpret-endpoint-actions cumulative-patch
                                 actions
                                 (struct-copy endpoint-group g
                                              [next-eid (+ new-eid 1)]
                                              [endpoints
                                               (hash-set (endpoint-group-endpoints g)
                                                         new-eid
                                                         new-ep)]
                                              [state (transition-state initial-transition)])
                                 new-eid
                                 (transition-actions initial-transition))]
    [(delete-endpoint eid)
     (interpret-endpoint-patch cumulative-patch
                               actions
                               (struct-copy endpoint-group g
                                            [endpoints
                                             (hash-remove (endpoint-group-endpoints g) eid)])
                               eid
                               (patch (matcher-empty) (pattern->matcher #t ?)))]))

(define (interpret-endpoint-actions cumulative-patch actions g eid unflattened-endpoint-actions)
  (define endpoint-actions (filter endpoint-action? (flatten unflattened-endpoint-actions)))
  (for/fold ([cumulative-patch cumulative-patch]
             [actions actions]
             [g g])
            ([endpoint-action endpoint-actions])
    (interpret-endpoint-action cumulative-patch
                               actions
                               g
                               eid
                               endpoint-action)))

(define (pretty-print-endpoint-group g [p (current-output-port)])
  (match-define (endpoint-group _ routing-table interests endpoints state) g)
  (fprintf p "ENDPOINT GROUP:\n")
  (fprintf p " ---- STATE:\n")
  (display (indented-port-output 6 (lambda (p) (prospect-pretty-print state p))) p)
  (newline p)
  (fprintf p " - ~a endpoints\n" (hash-count endpoints))
  (fprintf p " - routing table:\n")
  (pretty-print-matcher routing-table p))
