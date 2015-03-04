#lang racket/base
;; Core implementation of Incremental Network Calculus.

(provide )

(require racket/set)
(require racket/match)
(require (only-in racket/list flatten))
(require "functional-queue.rkt")
(require "route.rkt")
(require "patch.rkt")
(require "trace.rkt")
(module+ test (require rackunit))

;; Events = Patches ∪ Messages
(struct message (body) #:prefab)

;; Actions ⊃ Events
(struct quit () #:prefab)
(struct spawn (behavior boot) #:prefab)

;; Processes (machine states)
(struct process (interests behavior state) #:transparent)

;; A Behavior is a ((Option Event) Any -> Transition): a function
;; mapping an Event (or, in the #f case, a poll signal) and a
;; Process's current state to a Transition.
;;
;; A Transition is either
;;  - #f, a signal from a Process that it is inert and need not be
;;        scheduled until some Event relevant to it arrives; or,
;;  - a (transition Any (Constreeof Action)), a new Process state to
;;        be held by its World and a sequence of Actions for the World
;;        to take on the transitioning Process's behalf.
(struct transition (state actions) #:transparent)

;; A PID is a Nat.
;; A Label is a PID or 'meta.

;; VM private states
(struct world (next-pid ;; PID
               pending-action-queue ;; (Queueof (Cons Label Action))
               runnable-pids ;; (Setof PID)
               routing-table ;; (Matcherof (Setof Label))
               process-table ;; (HashTable PID Process)
               environment-interests ;; (Matcherof (set 'meta))
               ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event? x) (or (patch? x) (message? x)))
(define (action? x) (or (event? x) (spawn? x) (quit? x)))

(define (meta-label? x) (eq? x 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ensure-transition v)
  (if (or (not v) (transition? v))
      v
      (raise (exn:fail:contract (format "Expected transition (or #f); got ~v" v)
				(current-continuation-marks)))))

(define (clean-transition t)
  (and t (transition (transition-state t) (clean-actions (transition-actions t)))))

(define (clean-actions actions)
  (filter action? (flatten actions)))

(define (send-event e pid w)
  (match (hash-ref (world-process-table w) pid #f)
    [#f w]
    [(and p (process _ behavior old-state))
     (invoke-process pid
                     (lambda () (behavior e old-state))
                     (match-lambda
                       [#f w]
                       [(and t (transition new-state new-actions))
                        (trace-process-step e pid p #f t)
                        (update-process pid
                                        (struct-copy process p [state new-state])
                                        new-actions
                                        w)])
                     (lambda (exn)
                       (trace-process-step e pid p exn #f)
                       (enqueue-actions w pid (list (quit)))))]))

(define (update-process pid p actions w)
  (let* ((w (struct-copy world w [process-table (hash-set (world-process-table w) pid p)]))
         (w (mark-pid-runnable w pid)))
    (enqueue-actions w pid actions)))

(define (invoke-process pid thunk k-ok k-exn)
  (define-values (ok? result)
    (call-in-trace-context
     pid
     (lambda ()
       (with-handlers ([(lambda (exn) #t) (lambda (exn) (values #f exn))])
         (values #t (clean-transition
                     (ensure-transition
                      (with-continuation-mark 'minimart-process pid (thunk)))))))))
  (if ok?
      (k-ok result)
      (k-exn result)))

(define (mark-pid-runnable w pid)
  (struct-copy world w [runnable-pids (set-add (world-runnable-pids w) pid)]))

(define (enqueue-actions w label actions)
  (struct-copy world w
    [pending-action-queue
     (queue-append-list (world-pending-action-queue w)
                        (for/list [(a actions)] (cons label a)))]))

(define-syntax-rule (spawn-world boot-action ...)
  (make-spawn-world (lambda () (list boot-action ...))))

(define (make-world boot-actions)
  (world 0
         (list->queue (for/list ((a (in-list boot-actions))) (cons 'meta a)))
         (set)
         (matcher-empty)
         (hash)
         (matcher-empty)))

(define (make-spawn-world boot-actions-thunk)
  (spawn world-handle-event
         (lambda () (transition (make-world (boot-actions-thunk)) '()))))

(define (transition-bind k t0)
  (match-define (transition state0 actions0) t0)
  (match (k state0)
    [#f t0]
    [(transition state1 actions1) (transition state1 (cons actions0 actions1))]))

(define (sequence-transitions t0 . steps)
  (foldl transition-bind t0 steps))

(define (inert? w)
  (and (queue-empty? (world-pending-action-queue w))
       (set-empty? (world-runnable-pids w))))

(define (world-handle-event e w)
  (if (or e (not (inert? w)))
      (sequence-transitions (transition w '())
                            (inject-event e)
                            perform-actions ;; to process queued actions and the new "e"
                            perform-actions ;; to process responses to "e"
                            ;; ^ Double perform-actions makes it possible for children's
                            ;; responses to the incoming "e" be acted upon in the same
                            ;; event-handling cycle of the world.
                            step-children)
      (step-children w)))

(define ((inject-event e) w)
  (match e
    [#f w]
    [(? patch? delta) (enqueue-actions w 'meta (list (lift-patch delta)))]
    [(message body) (enqueue-actions w 'meta (list (message (at-meta body))))]))

(define (perform-actions w)
  (for/fold ([wt (transition (struct-copy world w [pending-action-queue (make-queue)]) '())])
      ((entry (in-list (queue->list (world-pending-action-queue w)))))
    (match-define [cons label a] entry)
    (define wt1 (transition-bind (perform-action label a) wt))
    (trace-internal-step label a (transition-state wt) wt1)
    wt1))

(define ((perform-action label a) w)
  (match a
    [(spawn behavior boot)
     (transition
      (invoke-process 'booting
                      boot
                      (match-lambda
                        [(transition initial-state initial-actions)
                         (define new-p (process (matcher-empty) behavior initial-state))
                         (define new-pid (world-next-pid w))
                         (update-process new-pid
                                         new-p
                                         initial-actions
                                         (struct-copy world w [next-pid (+ new-pid 1)]))])
                      (lambda (exn)
                        (log-error "Spawned process in world ~a died with exception:\n~a"
                                   (trace-pid-stack)
                                   (exn->string exn))
                        w))
      '())]
    [(quit)
     (define pt (world-process-table w))
     (match (hash-ref pt label)
       [#f (transition w '())]
       [(process interests _ _)
        (define delta (patch (matcher-empty) interests))
        (define new-w (struct-copy world w [process-table (hash-remove pt label)]))
        (apply-patch-in-world label delta new-w)])]
    [(? patch? delta-orig)
     (define p (hash-ref (world-process-table w) label))
     (cond
       [(or p (meta-label? label))
        (define old-interests (if (meta-label? label)
                                  (world-environment-interests w)
                                  (process-interests p)))
        (define delta (limit-patch (label-patch delta-orig label) old-interests))
        (define new-interests (apply-patch old-interests delta))
        (define new-w
          (if (meta-label? label)
              (struct-copy world w [environment-interests new-interests])
              (let* ((p (struct-copy process p [interests new-interests])))
                (struct-copy world w [process-table (hash-set (world-process-table w) label p)]))))
        (apply-patch-in-world label delta new-w)]
       [else ;; ignore actions for nonexistent processes
        (transition w '())])]
    [(and m (message body))
     (when (observe? body)
       (log-warning "Process ~a sent message containing query ~v"
                    (cons label (trace-pid-stack))
                    body))
     (cond
       [(matcher-match-value (world-routing-table w) body #f) ;; some other process has declared m
        (transition w '())]
       [else
        (define affected-pids (matcher-match-value (world-routing-table w) (observe body)))
        (transition (for/fold [(w w)] [(pid (in-set affected-pids))]
                      (send-event m pid w))
                    (and (not (meta-label? label))
                         (at-meta? body)
                         (message (at-meta-claim body))))])]))

(define (apply-patch-in-world label delta w)
  (define old-routing-table (world-routing-table w))
  (define new-routing-table (apply-patch old-routing-table delta))
  (define delta-aggregate (compute-aggregate-patch delta label old-routing-table))
  (define affected-pids (let ((pids (compute-affected-pids old-routing-table delta)))
                          (if (meta-label? label) pids (set-add pids label))))
  (transition (for/fold [(w (struct-copy world w [routing-table new-routing-table]))]
                        [(pid affected-pids)]
                (cond [(equal? pid label)
                       (define feedback
                         (patch (biased-intersection new-routing-table (patch-added delta))
                                (biased-intersection old-routing-table (patch-removed delta))))
                       (send-event feedback label w)]
                      [else
                       (define p (hash-ref (world-process-table w) pid))
                       (define event (view-patch delta-aggregate (process-interests p)))
                       (send-event event pid w)]))
              (and (not (meta-label? label))
                   (drop-patch delta-aggregate))))

(define (compute-affected-pids routing-table delta)
  (define cover (matcher-union (patch-added delta) (patch-removed delta)))
  (matcher-match-matcher cover routing-table
                         #:seed (set)
                         #:combiner (lambda (v1 v2 acc) (set-union v2 acc))))

(define (step-children w)
  (define runnable-pids (world-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; world is inert.
      (transition (for/fold [(w (struct-copy world w [runnable-pids (set)]))]
                            [(pid (in-set runnable-pids))]
                    (send-event #f pid w))
		  '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require racket/pretty)

  (define (step* w)
    (let loop ((w w) (actions '()))
      (pretty-print w)
      (match (world-handle-event #f w)
        [#f (values w (flatten actions))]
        [(transition new-w new-actions) (loop new-w (cons actions new-actions))])))

  (step* (make-world '()))
  )
