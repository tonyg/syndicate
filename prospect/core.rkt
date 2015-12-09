#lang racket/base
;; Core implementation of Incremental Network Calculus.

(provide (struct-out message)
         (except-out (struct-out quit) quit)
         (struct-out quit-world)
         (rename-out [quit <quit>])
         (except-out (struct-out spawn) spawn)
         (rename-out [spawn <spawn>])
         (struct-out transition)
         (struct-out world)

         (struct-out seal)

         (all-from-out "patch.rkt")

         ;; imported from route.rkt:
	 ?
	 wildcard?
	 ?!
	 (struct-out capture)
	 pretty-print-matcher
	 matcher->pretty-string
	 matcher-non-empty?
	 matcher-empty?
         matcher-empty
	 projection->pattern
         compile-projection
         matcher-project
         matcher-project/set
         matcher-project/set/single

         event?
         action?

         meta-label?

         prepend-at-meta
         assert
         retract
         sub
         unsub
         pub
         unpub

         (rename-out [make-quit quit])
         make-world
         spawn-world
         (rename-out [spawn-process spawn])
         spawn/stateless
         make-spawn-world

         transition-bind
         sequence-transitions

         world-handle-event
         clean-transition

         pretty-print-world)

(require racket/set)
(require racket/match)
(require (only-in racket/list flatten))
(require "functional-queue.rkt")
(require "route.rkt")
(require "patch.rkt")
(require "trace.rkt")
(require "mux.rkt")
(require "pretty.rkt")
(module+ test (require rackunit))

;; Events = Patches ∪ Messages
(struct message (body) #:prefab)

;; Actions ⊃ Events
(struct spawn (boot) #:prefab)
(struct quit-world () #:prefab) ;; NB. An action. Compare (quit), a Transition.

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
;;  - a (quit (Option Exn) (Constreeof Action)), signalling that the
;;        Process should never again be handed an event, and that any
;;        queued actions should be performed, followed by the sequence
;;        of Actions given, and then the process should be
;;        garbage-collected. The optional Exn is only used for
;;        debugging purposes; #f means normal termination.
(struct transition (state actions) #:transparent)
(struct quit (exn actions) #:prefab)

;; A PID is a Nat.
;; A Label is a PID or 'meta.

;; VM private states
(struct world (mux ;; Multiplexer
               pending-action-queue ;; (Queueof (Cons Label (U Action 'quit)))
               runnable-pids ;; (Setof PID)
               behaviors ;; (HashTable PID Behavior)
               states ;; (HashTable PID Any)
               )
  #:transparent
  #:methods gen:prospect-pretty-printable
  [(define (prospect-pretty-print w [p (current-output-port)])
     (pretty-print-world w p))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seals are used by protocols to prevent the routing tries from
;; examining internal structure of values.

(struct seal (contents)) ;; NB. Neither transparent nor prefab

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event? x) (or (patch? x) (message? x)))
(define (action? x) (or (event? x) (spawn? x) (quit-world? x)))

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

(define (general-transition? v)
  (or (not v) (transition? v) (quit? v)))

(define (ensure-transition v)
  (if (general-transition? v)
      v
      (raise (exn:fail:contract (format "Expected transition, quit or #f; got ~v" v)
				(current-continuation-marks)))))

(define (clean-transition t)
  (match t
    [#f #f]
    [(quit exn actions) (quit exn (clean-actions actions))]
    [(transition state actions) (transition state (clean-actions actions))]))

(define (clean-actions actions)
  (filter (lambda (x) (and (action? x) (not (patch-empty? x)))) (flatten actions)))

(define (send-event e pid w)
  (define behavior (hash-ref (world-behaviors w) pid #f))
  (define old-state (hash-ref (world-states w) pid #f))
  (if (not behavior)
      w
      (begin
        (trace-process-step e pid behavior old-state)
        (invoke-process pid
                        (lambda () (clean-transition (ensure-transition (behavior e old-state))))
                        (match-lambda
                          [#f w]
                          [(and q (quit exn final-actions))
                           (trace-process-step-result e pid behavior old-state exn q)
                           (enqueue-actions (disable-process pid exn w) pid (append final-actions
                                                                                    (list 'quit)))]
                          [(and t (transition new-state new-actions))
                           (trace-process-step-result e pid behavior old-state #f t)
                           (enqueue-actions (mark-pid-runnable (update-state w pid new-state) pid)
                                            pid
                                            new-actions)])
                        (lambda (exn)
                          (trace-process-step-result e pid behavior old-state exn #f)
                          (enqueue-actions (disable-process pid exn w) pid (list 'quit)))))))

(define (update-state w pid s)
  (struct-copy world w [states (hash-set (world-states w) pid s)]))

(define (send-event/guard delta pid w)
  (if (patch-empty? delta)
      w
      (send-event delta pid w)))

(define (disable-process pid exn w)
  (when exn
    (log-error "Process ~a died with exception:\n~a"
               (cons pid (trace-pid-stack))
               (exn->string exn)))
  (struct-copy world w
               [behaviors (hash-remove (world-behaviors w) pid)]
               [states (hash-remove (world-states w) pid)]))

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

(define (make-quit #:exception [exn #f] . actions)
  (quit exn actions))

(define-syntax-rule (spawn-process behavior-exp initial-state-exp initial-action-tree-exp)
  (spawn (lambda ()
           (list behavior-exp
                 (transition initial-state-exp initial-action-tree-exp)))))

(define-syntax-rule (spawn/stateless behavior-exp initial-action-tree-exp)
  (spawn-process (stateless-behavior-wrap behavior-exp)
                 (void)
                 initial-action-tree-exp))

(define ((stateless-behavior-wrap b) e state)
  (match (b e)
    [#f #f]
    [(? quit? q) q]
    [actions (transition state actions)]))

(define-syntax-rule (spawn-world boot-action ...)
  (make-spawn-world (lambda () (list boot-action ...))))

(define (make-world boot-actions)
  (world (mux)
         (list->queue (for/list ((a (in-list (clean-actions boot-actions)))) (cons 'meta a)))
         (set)
         (hash)
         (hash)))

(define (make-spawn-world boot-actions-thunk)
  (spawn (lambda ()
           (list world-handle-event
                 (transition (make-world (boot-actions-thunk)) '())))))

(define (transition-bind k t0)
  (match t0
    [#f (error 'transition-bind "Cannot bind from transition #f with continuation ~v" k)]
    [(quit _ _) t0]
    [(transition state0 actions0)
     (match (k state0)
       [#f t0]
       [(quit exn actions1) (quit exn (cons actions0 actions1))]
       [(transition state1 actions1) (transition state1 (cons actions0 actions1))])]))

(define (sequence-transitions t0 . steps)
  (sequence-transitions* t0 steps))

(define (sequence-transitions* t0 steps)
  (foldl transition-bind t0 steps))

(define (sequence-transitions0 state0 . steps)
  (sequence-transitions0* state0 steps))

(define (sequence-transitions0* state0 steps)
  (match steps
    ['() #f]
    [(cons step rest)
     (match (step state0)
       [#f (sequence-transitions0* state0 rest)]
       [(? quit? q) q]
       [(? transition? t) (sequence-transitions* t rest)])]))

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
    #:break (quit? wt) ;; TODO: should a quit action be delayed until the end of the turn?
    (match-define [cons label a] entry)
    (trace-internal-action label a (transition-state wt))
    (define wt1 (transition-bind (perform-action label a) wt))
    (trace-internal-action-result label a (transition-state wt) wt1)
    wt1))

(define ((perform-action label a) w)
  (match a
    [(spawn boot)
     (invoke-process 'booting
                     (lambda ()
                       (match (boot)
                         [(and results (list (? procedure?) (? general-transition?)))
                          results]
                         [other
                          (error 'spawn
                                 "Spawn boot procedure must yield boot spec; received ~v"
                                 other)]))
                     (lambda (results)
                       (match-define (list behavior initial-transition) results)
                       (create-process w behavior initial-transition))
                     (lambda (exn)
                       (log-error "Spawned process in world ~a died with exception:\n~a"
                                  (trace-pid-stack)
                                  (exn->string exn))
                       (transition w '())))]
    ['quit
     (define-values (new-mux _label delta delta-aggregate) (mux-remove-stream (world-mux w) label))
     ;; behavior & state in w already removed by disable-process
     (deliver-patches w new-mux label delta delta-aggregate)]
    [(quit-world)
     (make-quit)]
    [(? patch? delta-orig)
     (define-values (new-mux _label delta delta-aggregate)
       (mux-update-stream (world-mux w) label delta-orig))
     (deliver-patches w new-mux label delta delta-aggregate)]
    [(and m (message body))
     (when (observe? body)
       (log-warning "Stream ~a sent message containing query ~v"
                    (cons label (trace-pid-stack))
                    body))
     (if (and (not (meta-label? label)) ;; it's from a local process, not envt
              (at-meta? body)) ;; it relates to envt, not local
         (transition w (message (at-meta-claim body)))
         (transition (for/fold [(w w)]
                               [(pid (in-list (mux-route-message (world-mux w) body)))]
                       (send-event m pid w))
                     '()))]))

(define (create-process w behavior initial-transition)
  (if (not initial-transition)
      (transition w '()) ;; Uh, ok
      (let ()
        (define-values (postprocess initial-actions)
          (match (clean-transition initial-transition)
            [(and q (quit exn initial-actions0))
             (values (lambda (w pid)
                       (trace-process-step-result 'boot pid behavior (void) exn q)
                       (disable-process pid exn w))
                     (append initial-actions0 (list 'quit)))]
            [(and t (transition initial-state initial-actions0))
             (values (lambda (w pid)
                       (trace-process-step-result 'boot pid behavior (void) #f t)
                       (mark-pid-runnable (update-state w pid initial-state) pid))
                     initial-actions0)]))
        (define-values (initial-patch remaining-initial-actions)
          (match initial-actions
            [(cons (? patch? p) rest) (values p rest)]
            [other (values empty-patch other)]))
        (define-values (new-mux new-pid delta delta-aggregate)
          (mux-add-stream (world-mux w) initial-patch))
        (let* ((w (struct-copy world w
                               [behaviors (hash-set (world-behaviors w)
                                                    new-pid
                                                    behavior)]))
               (w (enqueue-actions (postprocess w new-pid) new-pid remaining-initial-actions)))
          (deliver-patches w new-mux new-pid delta delta-aggregate)))))

(define (deliver-patches w new-mux acting-label delta delta-aggregate)
  (define-values (patches meta-action)
    (compute-patches (world-mux w) new-mux acting-label delta delta-aggregate))
  (transition (for/fold [(w (struct-copy world w [mux new-mux]))]
                        [(entry (in-list patches))]
                (match-define (cons label event) entry)
                (send-event/guard event label w))
              meta-action))

(define (step-children w)
  (define runnable-pids (world-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; world is inert.
      (transition (for/fold [(w (struct-copy world w [runnable-pids (set)]))]
                            [(pid (in-set runnable-pids))]
                    (send-event #f pid w))
		  '())))

(define (pretty-print-world w [p (current-output-port)])
  (local-require racket/pretty)
  (match-define (world mux qs runnable behaviors states) w)
  (fprintf p "WORLD:\n")
  (fprintf p " - ~a queued actions\n" (queue-length qs))
  (fprintf p " - ~a runnable pids ~a\n" (set-count runnable) (set->list runnable))
  (fprintf p " - ~a live processes (~a with claims)\n"
           (hash-count states)
           (hash-count (mux-interest-table mux)))
  (fprintf p " - next pid: ~a\n" (mux-next-pid mux))
  (fprintf p " - routing table:\n")
  (pretty-print-matcher (mux-routing-table mux) p)
  (for ([pid (set-union (hash-keys (mux-interest-table mux)) (hash-keys states))])
    (fprintf p " ---- process ~a, behavior ~v, STATE:\n" pid (hash-ref behaviors pid #f))
    (define state (hash-ref states pid #f))
    (display (indented-port-output 6 (lambda (p) (prospect-pretty-print state p))) p)
    (newline p)
    (fprintf p "      process ~a, behavior ~v, CLAIMS:\n" pid (hash-ref behaviors pid #f))
    (display (indented-port-output 6 (lambda (p)
                                       (pretty-print-matcher (mux-interests-of mux pid) p)))
             p)
    (newline p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require racket/pretty)

  (define (step* w)
    (let loop ((w w) (actions '()))
      (pretty-print w)
      (match (world-handle-event #f w)
        [#f (values w #f (flatten actions))]
        [(quit exn new-actions) (values w exn (flatten (cons actions new-actions)))]
        [(transition new-w new-actions) (loop new-w (cons actions new-actions))])))

  (step* (make-world '()))
  )
