#lang syndicate

(provide supervisor
         spawn-supervisor-with
         ONE-FOR-ONE
         ONE-FOR-ALL
         REST-FOR-ONE
         (struct-out child-spec))

(require (only-in "monitor.rkt" assertion-added assertion-removed)
         "store.rkt"
         (only-in "trie.rkt" trie-union trie-union-all pattern->trie)
         (submod "actor.rkt" implementation-details)
         "util.rkt"
         syntax/parse/define)

(assertion-struct up (name))
(assertion-struct stay-up (name))

;; a Strategy is one of
;;  - 'one-for-one
;;  - 'one-for-all
;;  - 'rest-for-one
(define-values (ONE-FOR-ONE ONE-FOR-ALL REST-FOR-ONE)
  (values 'one-for-one 'one-for-all 'rest-for-one))

;; a ChildSpec is a
;;  (child-spec name boot)
(struct child-spec (name boot) #:prefab)

(define (supervisor children
                    #:strategy [strategy ONE-FOR-ONE]
                    #:intensity [intensity 3]
                    #:period [period 5000] ;; milliseconds
                    #:name [my-name (gensym 'supervisor)])
  (spawn
    (define supervisor-root (current-facet-id))
    (assert (up my-name))
    (field [restarts '()])

    (define (add-restart!)
      (restarts (cons (current-inexact-milliseconds) (restarts))))

    (define (request-restart! child-name)
      (match (should-restart-with? (restarts) intensity period)
        [(? list? recent-restarts)
         (restarts recent-restarts)
         (add-restart!)
         #t]
        [_
         (log-error "Supervisor ~a reached maximum restart intensity, shutting down" my-name)
         (stop-facet supervisor-root)
         #f]))
    (on-start
     (for ([cs children])
       (supervise my-name cs request-restart!)))))


(define (should-restart-with? restarts intensity period)
  (define relevant (relevant-restarts restarts period))
  (cond
    [(< (length relevant) intensity)
     relevant]
    [else
     #f]))

(define (relevant-restarts restarts period)
  (define now (current-inexact-milliseconds))
  (define oldest-to-keep (- now period))
  (filter (lambda (r) (>= r oldest-to-keep)) restarts))

(define (supervise my-name the-child-spec request-restart!)
  (react
    (define child-name (child-spec-name the-child-spec))
    (define child-boot (child-spec-boot the-child-spec))
    (assert (stay-up child-name))
    (define/query-value up? #f (up child-name) #t)

    (define (boot)
      (spawn/link my-name child-name child-boot)
      ;; I *think* that this suspends the facet, not the whole actor
      (flush!)
      (unless (up?)
        (log-error "Child process ~a of supervisor ~a failed to boot. Retrying" child-name my-name)
        (when (request-restart! child-name)
          (boot))))

    (on-start (boot))

    (on (retracted (up child-name))
        (log-error "Child process ~a of supervisor ~a went down. Attempting restart" child-name my-name)
        (when (request-restart! child-name)
              (boot)))
    ))

(define (spawn/link supervisor-name actor-name actor-thunk)
  (flush-pending-patch!)
  (with-store [(current-action-transformer
                (wrap-supervise-actor supervisor-name actor-name (current-action-transformer)))]
    (actor-thunk)))

(define ((wrap-supervise-actor supervisor-name actor-name previous-action-transformer) ac)
  (match (previous-action-transformer ac)
    [(<actor> boot initial-assertions)
     (printf "wrapping spawned actor with supervision\n")
     (define boot* (lambda ()
                     (match-define (list inner-behavior txn name) (boot))
                     (list (supervised-actor-behavior supervisor-name actor-name inner-behavior)
                           txn
                           name)))
     (define sup (supervision-assertions supervisor-name actor-name))
     (define assertions* (trie-union initial-assertions sup))
     (<actor> boot* assertions*)]
    [act
     (log-error "Provided a specification for supervision that did not spawn an actor: ~a" act)]))

(define ((supervised-actor-behavior supervisor-name actor-name inner-behavior) evt st)
  (match evt
    [(assertion-removed (up supervisor-name))
     (log-error "supervisor of actor ~a no longer detected, terminating; event:\n ~a" actor-name evt)
     (quit)]
    [(assertion-removed (stay-up actor-name))
     (log-error "supervisor of actor ~a requesting termination, exiting; event:\n ~a" actor-name evt)
     (quit)]
    [_
     ;; TODO - could consider supervising spawned actors
     (inner-behavior evt st)]))


(define (supervision-assertions supervisor-name actor-name)
  (trie-union-all (list (pattern->trie '<supervision>
                                       (up actor-name))
                        (pattern->trie '<supervision>
                                       (observe (up supervisor-name)))
                        (pattern->trie '<supervision>
                                       (observe (stay-up actor-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Form: Directly Spawn Actors in Body

(begin-for-syntax
  (define-splicing-syntax-class supervisor-opts
    (pattern (~seq (~alt (~optional (~seq #:strategy strategy))
                         (~optional (~seq #:intensity intensity))
                         (~optional (~seq #:period period))
                         (~optional (~seq #:name name)))
                   ...)
             #:attr call #'((~@ (~? (~@ #:strategy strategy))
                                (~? (~@ #:intensity intensity))
                                (~? (~@ #:period period))
                                (~? (~@ #:name name))))
             )))

(define-syntax-parser spawn-supervisor-with
  [(_ opts:supervisor-opts
      body-expr:expr ...+)
   (quasisyntax/loc this-syntax
     (let ([children (determine-initial-children (lambda () body-expr ...))])
       (supervisor children #,@#'opts.call)))])

(define (determine-initial-children boot)
  (define spawn-acts (capture-spawn-actions 'spawn-supervisor-with boot))
  (for/list ([act spawn-acts])
    ;; unfortunately I don't see a good way to get the actor's name without calling its boot procedure
    (child-spec (gensym 'supervised-actor)
                (lambda () (perform-actions! (list act))))))

