#lang prospect

(require racket/set)

;; `binding` tuples associate keys with values at a certain version.
;; versions start at 0 and increase by 1 with every successful update.
(struct binding (key version value) #:transparent)

;; `update` tuples request a binding update.
(struct update (binding) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define observation-projector (compile-projection (observe (binding (?!) ? ?))))
(define update-projector (compile-projection (update (?! (binding ? ? ?)))))

(struct db-state (bindings observed-keys) #:transparent)

(define (lookup-binding bindings key)
  (hash-ref bindings key (lambda () (binding key -1 (void)))))

(define ((process-suggestion suggestion) state)
  (match-define (db-state bindings observed-keys) state)
  (match-define (binding key suggested-version _) suggestion)
  (define old-binding (lookup-binding bindings key))
  (match-define (binding _ current-version _) old-binding)
  (if (= suggested-version (+ current-version 1))
      (transition (struct-copy db-state state
                               [bindings (hash-set bindings key suggestion)])
                  (when (set-member? observed-keys key)
                    (patch-seq (retract old-binding)
                               (assert suggestion))))
      (transition state '())))

(define ((adjust-observations added-observations removed-observations) state)
  (match-define (db-state bindings observed-keys) state)
  (transition (struct-copy db-state state
                           [observed-keys (set-union added-observations
                                                     (set-subtract observed-keys
                                                                   removed-observations))])
              (list (for/list [(key (in-set removed-observations))]
                      (retract (binding key ? ?)))
                    (for/list [(key (in-set added-observations))]
                      (when (hash-has-key? bindings key)
                        (assert (lookup-binding bindings key)))))))

(define (db)
  (spawn (lambda (e old-state)
           (match e
             [(? patch? p)
              (define-values (added-observations removed-observations)
                (patch-project/set/single p observation-projector))
              (define added-updates (matcher-project/set/single (patch-added p) update-projector))
              (transition-bind (adjust-observations added-observations removed-observations)
                               (for/fold [(t (transition old-state '()))]
                                         [(suggestion (in-set added-updates))]
                                 (transition-bind (process-suggestion suggestion) t)))]
             [_ #f]))
         (db-state (hash) (set))
         (sub (observe (binding ? ? ?)))
         (sub (update (binding ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binding-projector (compile-projection (?! (binding ? ? ?))))

(define (async-update key version value on-complete on-conflict)
  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (match (set->list (matcher-project/set/single (patch-added p) binding-projector))
                ['() #f]
                [(list (binding _ (== version) (== value)))
                 (quit (on-complete))]
                [(list (binding _ (== (- version 1)) _))
                 #f]
                [(list (binding _ other-version other-value))
                 (quit (on-conflict key version value other-version other-value))])]
             [_ #f]))
         (void)
         (assert (update (binding key version value)))
         (sub (binding key ? ?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(db)

(define (monitor key)
  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (define n (matcher-project/set/single (patch-added p) (compile-projection (?!))))
              (for [(b n)] (printf "binding update: ~v\n" b))
              #f]
             [_ #f]))
         (void)
         (sub (binding key ? ?))))

(monitor 'a)
(monitor 'b)

(async-update 'a 0 0
              (lambda () (async-update 'a 1 1
                                       (lambda () '())
                                       (lambda args (error 'conflict "at a/1"))))
              (lambda args (error 'conflict "at a/0")))
(async-update 'b 0 0
              (lambda () (async-update 'b 0 1
                                       (lambda () '())
                                       (lambda args '())))
              (lambda args (error 'conflict "at b/0")))
