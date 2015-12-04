#lang racket/base
;; Marketplace-style endpoints (analogous to threads)

(provide (struct-out endpoint-group)
         (struct-out add-endpoint)
         (struct-out delete-endpoint)
         (struct-out as-endpoint)
         make-endpoint-group
         spawn-endpoint-group
         boot-endpoint-group
         endpoint-action?
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
(struct endpoint-group (mux ;; Mux
                        endpoints ;; (HashTable EID Endpoint)
                        state ;; Any
                        )
  #:transparent
  #:methods gen:prospect-pretty-printable
  [(define (prospect-pretty-print g [p (current-output-port)])
     (pretty-print-endpoint-group g p))])

;; A Endpoint is a (Event State -> Transition)
;; A Transition reuses the struct from core, but with EndpointActions instead of plain Actions.
;; An EndpointAction is either an Action, or a
;; (add-endpoint (EID State -> (Values Endpoint Transition))), or a
;; (delete-endpoint)
;; (as-endpoint EID EndpointAction)
(struct add-endpoint (function) #:prefab)
(struct delete-endpoint () #:prefab)
(struct as-endpoint (eid action) #:prefab)

(define (make-endpoint-group initial-state)
  (endpoint-group (mux)
                  (hash)
                  initial-state))

(define-syntax-rule (spawn-endpoint-group initial-state action-constree ...)
  (<spawn> (lambda () (boot-endpoint-group initial-state (list action-constree ...)))))

(define (boot-endpoint-group initial-state initial-actions)
  (define-values (final-cumulative-patch final-actions final-g)
    (interpret-endpoint-actions empty-patch
                                '()
                                (make-endpoint-group initial-state)
                                -1
                                initial-actions))
  (list endpoint-group-handle-event
        (transition final-g (incorporate-cumulative-patch final-actions
                                                          final-cumulative-patch))))

(define (endpoint-action? a)
  (or (action? a)
      (add-endpoint? a)
      (delete-endpoint? a)
      (and (as-endpoint? a) (endpoint-action? (as-endpoint-action a)))))

(define (inert-endpoint e state) #f)

(define (endpoint-group-handle-event e g)
  (match-define (endpoint-group m endpoints state) g)
  (define affected-eids
    (match e
      [#f (hash-keys endpoints)]
      [(? patch?) (compute-affected-pids (mux-routing-table m) e)]
      [(message body) (mux-route-message m body)]))
  (sequence-handlers g (for/list [(eid (sort affected-eids <))]
                         (list (if (patch? e)
                                   (view-patch e (mux-interests-of m eid))
                                   e)
                               eid
                               (hash-ref endpoints eid (lambda () inert-endpoint))))))

(define (sequence-handlers g tasks)
  (let/ec return
    (define-values (final-cumulative-patch final-actions final-g idle?)
      (for/fold ([cumulative-patch empty-patch]
                 [actions '()]
                 [g g]
                 [idle? #t])
                ([task tasks])
        (match-define (list e eid ep) task)
        (match (ep e (endpoint-group-state g))
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
  (define-values (new-mux _eid p p-aggregate)
    (mux-update-stream (endpoint-group-mux g) eid p0))
  (values (patch-seq cumulative-patch p-aggregate)
          actions
          (struct-copy endpoint-group g [mux new-mux])))

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
     (define-values (new-mux new-eid _p _p-aggregate)
       (mux-add-stream (endpoint-group-mux g) empty-patch))
     (define-values (new-ep initial-transition) (function new-eid (endpoint-group-state g)))
     (interpret-endpoint-actions cumulative-patch
                                 actions
                                 (struct-copy endpoint-group g
                                              [mux new-mux]
                                              [endpoints
                                               (hash-set (endpoint-group-endpoints g)
                                                         new-eid
                                                         new-ep)]
                                              [state (transition-state initial-transition)])
                                 new-eid
                                 (transition-actions initial-transition))]
    [(delete-endpoint)
     (interpret-endpoint-patch cumulative-patch
                               actions
                               (struct-copy endpoint-group g
                                            [endpoints
                                             (hash-remove (endpoint-group-endpoints g) eid)])
                               eid
                               (patch (matcher-empty) (pattern->matcher #t ?)))]
    [(as-endpoint other-eid inner-endpoint-action)
     (interpret-endpoint-actions cumulative-patch actions g other-eid inner-endpoint-action)]))

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
  (match-define (endpoint-group m endpoints state) g)
  (fprintf p "ENDPOINT GROUP:\n")
  (fprintf p " ---- STATE:\n")
  (display (indented-port-output 6 (lambda (p) (prospect-pretty-print state p))) p)
  (newline p)
  (fprintf p " - ~a endpoints\n" (hash-count endpoints))
  (fprintf p " - next eid: ~a\n" (mux-next-pid mux))
  (fprintf p " - routing table:\n")
  (pretty-print-matcher (mux-routing-table mux) p))
