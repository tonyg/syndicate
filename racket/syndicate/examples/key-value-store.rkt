#lang syndicate

(require racket/set)

;; Bindings are versioned with a pair of an epoch and a version.
;; Within an epoch, versions increase monotonically.
;; Epochs increase monotonically. At an epoch boundary, versions reset to 0.

;; Indicates a nonexistent binding when a binding has this as its value.
;; (I guess bindings with `(absent)` as their value are more properly pseudo-bindings.)
(struct absent () #:transparent)

;; `binding` tuples associate keys with values at a certain version.
;; versions start at 0 and increase by 1 with every successful update.
(struct binding (key epoch version value) #:transparent)

;; `update` tuples request a binding update.
;; The epoch and version describe the *current* version of the binding.
(struct update (key base-epoch base-version value) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define observation-projector (observe (binding (?!) ? ? ?)))
(define update-projector (?! (update ? ? ? ?)))

(struct db-state (epoch bindings observed-keys) #:transparent)

(define (lookup-binding epoch bindings key)
  (hash-ref bindings key (lambda () (binding key epoch 0 (absent)))))

(define ((process-suggestion suggestion) state)
  (match-define (db-state epoch bindings observed-keys) state)
  (match-define (update key base-epoch base-version new-value) suggestion)
  (define old-binding (lookup-binding epoch bindings key))
  (match-define (binding _ current-epoch current-version _) old-binding)
  (if (and (= current-epoch base-epoch)
           (= current-version base-version))
      (let ((new-binding (cond
                           [(absent? new-value) (binding key (+ epoch 1) 0 new-value)]
                           [(> epoch current-epoch) (binding key epoch 0 new-value)]
                           [else (binding key base-epoch (+ base-version 1) new-value)])))
        (transition (struct-copy db-state state
                                 [epoch (binding-epoch new-binding)]
                                 [bindings (if (absent? new-value)
                                               (hash-remove bindings key)
                                               (hash-set bindings key new-binding))])
                    (when (set-member? observed-keys key)
                      (patch-seq (retract old-binding) (assert new-binding)))))
      (transition state '())))

(define ((adjust-observations added-observations removed-observations) state)
  (match-define (db-state epoch bindings observed-keys) state)
  (transition (struct-copy db-state state
                           [observed-keys (set-union added-observations
                                                     (set-subtract observed-keys
                                                                   removed-observations))])
              (list (for/list [(key (in-set removed-observations))]
                      (retract (binding key ? ? ?)))
                    (for/list [(key (in-set added-observations))]
                      (assert (lookup-binding epoch bindings key))))))

(define (db)
  (actor (lambda (e old-state)
           (match e
             [(? patch? p)
              (define-values (added-observations removed-observations)
                (patch-project/set/single p observation-projector))
              (define added-updates (trie-project/set/single (patch-added p) update-projector))
              (transition-bind (adjust-observations added-observations removed-observations)
                               (for/fold [(t (transition old-state '()))]
                                         [(suggestion (in-set added-updates))]
                                 (transition-bind (process-suggestion suggestion) t)))]
             [_ #f]))
         (db-state 0 (hash) (set))
         (patch-seq (sub (observe (binding ? ? ? ?)))
                    (sub (update ? ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binding-projector (?! (binding ? ? ? ?)))

(define (async-update key epoch version value on-complete on-conflict)
  (actor (lambda (e s)
           (match e
             [(? patch? p)
              (match (set->list (trie-project/set/single (patch-added p) binding-projector))
                ['() #f]
                [(list (binding _ (== epoch) (== version) _)) #f]
                [(list (binding _ (== epoch) (== (+ version 1)) (== value))) (quit (on-complete))]
                [(list (binding _ new-epoch 0 (== value)))
                 #:when (> new-epoch epoch)
                 (quit (on-complete))]
                [(list (binding _ other-epoch other-version other-value))
                 (quit (on-conflict key
                                    epoch version value
                                    other-epoch other-version other-value))])]
             [_ #f]))
         (void)
         (patch-seq (assert (update key epoch version value))
                    (sub (binding key ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(db)

(define (monitor key)
  (actor (lambda (e s)
           (match e
             [(? patch? p)
              (define n (project-assertions (patch-added p) (?!)))
              (for [(b n)] (printf "binding update: ~v\n" b))
              #f]
             [_ #f]))
         (void)
         (sub (binding key ? ? ?))))

(monitor 'a)
(monitor 'b)

(define (now-update-a-again)
  (async-update 'a 0 2 2
                (lambda () '())
                (on-conflict "a/2")))

(define ((on-conflict where) . args)
  (error 'conflict "at ~v" where))

(async-update 'a 0 0 0
              (lambda () (async-update 'a 0 1 1
                                       (lambda () '())
                                       (on-conflict "a/1")))
              (on-conflict "a/0"))
(async-update 'b 0 0 0
              (lambda () (async-update 'b 0 0 1
                                       (lambda () '())
                                       (lambda args
                                         (async-update 'b 0 1 (absent)
                                                       now-update-a-again
                                                       (on-conflict "b/2")))))
              (on-conflict "b/0"))
