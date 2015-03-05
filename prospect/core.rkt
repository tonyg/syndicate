#lang racket/base
;; Core implementation of Incremental Network Calculus.

(provide (struct-out message)
         (struct-out quit)
         (except-out (struct-out spawn) spawn)
         (rename-out [spawn <spawn>])
         (struct-out process)
         (struct-out transition)
         (struct-out world)

         (all-from-out "patch.rkt")

         ;; imported from route.rkt:
	 ?
	 wildcard?
	 ?!
	 (struct-out capture)
	 pretty-print-matcher
	 matcher->pretty-string
	 matcher-empty?
         matcher-empty
	 projection->pattern
         compile-projection

         event?
         action?

         meta-label?

         assert
         retract
         sub
         unsub
         pub
         unpub

         make-world
         spawn-world
         (rename-out [spawn-process spawn])
         make-spawn-world

         world-handle-event
         clean-transition
)

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
(struct spawn (boot) #:prefab)

;; Processes (machine states): (process Matcher (Option Behavior) Any)
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

(define (prepend-at-meta pattern level)
  (if (zero? level)
      pattern
      (at-meta (prepend-at-meta pattern (- level 1)))))

(define (observe-at-meta pattern level)
  (if (zero? level)
      (pattern->matcher #t (observe pattern))
      (matcher-union
       (pattern->matcher #t (observe (prepend-at-meta pattern level)))
       (pattern->matcher #t (at-meta (embedded-matcher (observe-at-meta pattern (- level 1))))))))

(define (assert pattern #:meta-level [level 0])
  (patch (pattern->matcher #t (prepend-at-meta pattern level)) (matcher-empty)))
(define (retract pattern #:meta-level [level 0])
  (patch (matcher-empty) (pattern->matcher #t (prepend-at-meta pattern level))))

(define (sub pattern #:meta-level [level 0])
  (patch (observe-at-meta pattern level) (matcher-empty)))
(define (unsub pattern #:meta-level [level 0])
  (patch (matcher-empty) (observe-at-meta pattern level)))

(define (pub pattern #:meta-level [level 0]) (assert (advertise pattern) #:meta-level level))
(define (unpub pattern #:meta-level [level 0]) (retract (advertise pattern) #:meta-level level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ensure-transition v)
  (if (or (not v) (transition? v))
      v
      (raise (exn:fail:contract (format "Expected transition (or #f); got ~v" v)
				(current-continuation-marks)))))

(define (clean-transition t)
  (and t (transition (transition-state t) (clean-actions (transition-actions t)))))

(define (clean-actions actions)
  (filter (lambda (x) (and (action? x) (not (patch-empty? x)))) (flatten actions)))

(define (send-event e pid w)
  (match (hash-ref (world-process-table w) pid #f)
    [#f w]
    [(process _ #f _) w] ;; disabled due to earlier error
    [(and p (process _ behavior old-state))
     (invoke-process pid
                     (lambda () (clean-transition (ensure-transition (behavior e old-state))))
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
                       (enqueue-actions (disable-process pid exn w) pid (list (quit)))))]))

(define (send-event/guard delta pid w)
  (if (patch-empty? delta)
      w
      (send-event delta pid w)))

(define (disable-process pid exn w)
  (log-error "Process ~a died with exception:\n~a" (cons pid (trace-pid-stack)) (exn->string exn))
  (match (hash-ref (world-process-table w) pid #f)
    [#f w]
    [old-p
     (define new-p (struct-copy process old-p [behavior #f]))
     (struct-copy world w [process-table (hash-set (world-process-table w) pid new-p)])]))

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
         (values #t (with-continuation-mark 'minimart-process pid (thunk)))))))
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

(define-syntax-rule (spawn-process behavior-exp initial-state-exp initial-patch-exp ...)
  (spawn (lambda (pid)
           (process (apply-patch (matcher-empty)
                                 (label-patch (patch-seq initial-patch-exp ...) (set pid)))
                    behavior-exp
                    initial-state-exp))))

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
  (spawn (lambda (pid)
           (process (matcher-empty)
                    world-handle-event
                    (make-world (boot-actions-thunk))))))

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
                            perform-actions
                            (lambda (w) (or (step-children w) (transition w '()))))
      (step-children w)))

(define ((inject-event e) w)
  (transition (match e
                [#f w]
                [(? patch? delta) (enqueue-actions w 'meta (list (lift-patch delta)))]
                [(message body) (enqueue-actions w 'meta (list (message (at-meta body))))])
              '()))

(define (perform-actions w)
  (for/fold ([wt (transition (struct-copy world w [pending-action-queue (make-queue)]) '())])
      ((entry (in-list (queue->list (world-pending-action-queue w)))))
    (match-define [cons label a] entry)
    (define wt1 (transition-bind (perform-action label a) wt))
    (trace-internal-step label a (transition-state wt) wt1)
    wt1))

(define ((perform-action label a) w)
  (match a
    [(spawn boot)
     (define new-pid (world-next-pid w))
     (invoke-process 'booting
                     (lambda ()
                       (match (boot new-pid)
                         [(? process? p) p]
                         [other (error 'spawn
                                       "Spawn boot procedure must yield process; received ~v"
                                       other)]))
                     (lambda (new-p)
                       (define new-interests (process-interests new-p))
                       (define new-w
                         (update-process new-pid
                                         new-p
                                         '()
                                         (struct-copy world w [next-pid (+ new-pid 1)])))
                       (apply-patch-in-world new-pid (patch new-interests (matcher-empty)) new-w))
                     (lambda (exn)
                       (log-error "Spawned process in world ~a died with exception:\n~a"
                                  (trace-pid-stack)
                                  (exn->string exn))
                       (transition w '())))]
    [(quit)
     (define pt (world-process-table w))
     (match (hash-ref pt label #f)
       [#f (transition w '())]
       [(process interests _ _)
        (define delta (patch (matcher-empty) interests))
        (define-values (discarded-actions retained-actions)
          (queue-partition (lambda (e) (equal? (car e) label)) (world-pending-action-queue w)))
        (when (not (queue-empty? discarded-actions))
          (log-error "Process ~a had ~a queued actions at exit"
                     label
                     (queue-length discarded-actions)))
        (define new-w (struct-copy world w
                                   [process-table (hash-remove pt label)]
                                   [pending-action-queue retained-actions]))
        (apply-patch-in-world label delta new-w)])]
    [(? patch? delta-orig)
     (define p (hash-ref (world-process-table w) label #f))
     (cond
       [(or p (meta-label? label))
        (define old-interests (if (meta-label? label)
                                  (world-environment-interests w)
                                  (process-interests p)))
        (define delta (limit-patch (label-patch delta-orig (set label)) old-interests))
        (define new-interests (apply-patch old-interests delta))
        (define new-w
          (if (meta-label? label)
              (struct-copy world w [environment-interests new-interests])
              (let* ((p (struct-copy process p [interests new-interests])))
                (struct-copy world w [process-table (hash-set (world-process-table w) label p)]))))
        (apply-patch-in-world label delta new-w)]
       [else ;; we can still apply actions for nonexistent processes,
             ;; but we can't limit the patches, making their zombie
             ;; patch actions potentially less efficient.
        (apply-patch-in-world label delta-orig w)])]
    [(and m (message body))
     (when (observe? body)
       (log-warning "Process ~a sent message containing query ~v"
                    (cons label (trace-pid-stack))
                    body))
     (cond
       [(matcher-match-value (world-routing-table w) body #f) ;; some other process has declared m
        (transition w '())]
       [else
        (define local-to-meta? (and (not (meta-label? label)) ;; it's from a local process, not envt
                                    (at-meta? body))) ;; it relates to envt, not local
        (define affected-pids (if local-to-meta?
                                  (set)
                                  (matcher-match-value (world-routing-table w) (observe body))))
        (transition (for/fold [(w w)] [(pid (in-set affected-pids))]
                      (send-event m pid w))
                    (and local-to-meta?
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
                       (send-event/guard feedback label w)]
                      [else
                       (define p (hash-ref (world-process-table w) pid))
                       (define event (view-patch delta-aggregate (process-interests p)))
                       (send-event/guard event pid w)]))
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
