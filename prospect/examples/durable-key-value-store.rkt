#lang prospect

(require racket/set)
(require racket/file)

;; Bindings are versioned with a pair of an epoch and a version.
;; Within an epoch, versions increase monotonically.
;; Epochs increase monotonically. At an epoch boundary, versions reset to 0.

;; Indicates a nonexistent binding when a binding has this as its value.
;; (I guess bindings with `(absent)` as their value are more properly pseudo-bindings.)
(struct absent () #:prefab)

;; `binding` tuples associate keys with values at a certain version.
;; versions start at 0 and increase by 1 with every successful update.
(struct binding (key epoch version value) #:prefab)

;; `update` tuples request a binding update.
;; The epoch and version describe the *current* version of the binding.
(struct update (key base-epoch base-version value) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define observation-projector (compile-projection (observe (binding (?!) ? ? ?))))
(define update-projector (compile-projection (?! (update ? ? ? ?))))

(struct db-state (epoch directory observed-keys) #:transparent)

(define (key->filename directory key)
  (build-path directory (string-append (symbol->string key) ".rktd")))

(define (lookup-binding epoch directory key)
  (with-handlers ((exn:fail:filesystem? (lambda (e) (binding key epoch 0 (absent)))))
    (file->value (key->filename directory key))))

(define (directory->epoch-file-path directory)
  (build-path directory "epoch"))

(define (save-epoch! directory epoch)
  (write-to-file epoch (directory->epoch-file-path directory) #:exists 'replace))

(define (load-epoch directory)
  (file->value (directory->epoch-file-path directory)))

(define (setup-directory! directory)
  (make-directory* directory)
  (when (not (file-exists? (directory->epoch-file-path directory)))
    (save-epoch! directory 0)))

(define ((process-suggestion suggestion) state)
  (match-define (db-state epoch directory observed-keys) state)
  (match-define (update key base-epoch base-version new-value) suggestion)
  (define old-binding (lookup-binding epoch directory key))
  (match-define (binding _ current-epoch current-version _) old-binding)
  (if (and (= current-epoch base-epoch)
           (= current-version base-version))
      (let ((new-binding (cond
                           [(absent? new-value)
                            (save-epoch! directory (+ epoch 1))
                            (binding key (+ epoch 1) 0 new-value)]
                           [(> epoch current-epoch) (binding key epoch 0 new-value)]
                           [else (binding key epoch (+ base-version 1) new-value)])))
        (if (absent? new-value)
            (with-handlers ((exn:fail:filesystem? void))
              (delete-file (key->filename directory key)))
            (write-to-file new-binding (key->filename directory key) #:exists 'replace))
        (transition (struct-copy db-state state [epoch (binding-epoch new-binding)])
                    (when (set-member? observed-keys key)
                      (patch-seq (retract old-binding) (assert new-binding)))))
      (transition state '())))

(define ((adjust-observations added-observations removed-observations) state)
  (match-define (db-state epoch directory observed-keys) state)
  (transition (struct-copy db-state state
                           [observed-keys (set-union added-observations
                                                     (set-subtract observed-keys
                                                                   removed-observations))])
              (list (for/list [(key (in-set removed-observations))]
                      (retract (binding key ? ? ?)))
                    (for/list [(key (in-set added-observations))]
                      (assert (lookup-binding epoch directory key))))))

(define (db directory)
  (setup-directory! directory)
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
         (db-state (load-epoch directory) directory (set))
         (patch-seq (sub (observe (binding ? ? ? ?)))
                    (sub (update ? ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binding-projector (compile-projection (?! (binding ? ? ? ?))))

(define (async-update key epoch version value on-complete on-conflict)
  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (match (set->list (matcher-project/set/single (patch-added p) binding-projector))
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

(db "/tmp/durable-key-value-store")

(define (monitor key)
  (spawn (lambda (e s)
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
