#lang racket/base
;; Core implementation of Incremental Network Calculus.

(provide (struct-out message)
         (except-out (struct-out quit) quit)
         (struct-out quit-network)
         (rename-out [quit <quit>])
         (except-out (struct-out spawn) spawn)
         (rename-out [spawn <spawn>])
         (struct-out transition)
         (struct-out network)

         (struct-out seal)
         clean-actions

         (all-from-out "patch.rkt")

         ;; imported from route.rkt:
	 ?
	 wildcard?
	 ?!
	 (struct-out capture)
	 pretty-print-trie
	 trie->pretty-string
	 trie-non-empty?
	 trie-empty?
         trie-empty
	 projection->pattern
         compile-projection
         trie-project
         trie-project/set
         trie-project/set/single
         project-assertions

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
         spawn-network
         (rename-out [spawn-process spawn])
         spawn/stateless
         make-spawn-network

         transition-bind
         sequence-transitions
         sequence-transitions*
         sequence-transitions0
         sequence-transitions0*

         clean-transition

         fork-network
         pretty-print-network)

(require racket/set)
(require racket/match)
(require (only-in racket/list flatten))
(require "functional-queue.rkt")
(require "route.rkt")
(require "patch.rkt")
(require "trace.rkt")
(require "mux.rkt")
(require "pretty.rkt")
(require racket/async-channel)
(module+ test (require rackunit))

;; Events = Patches ∪ Messages
(struct message (body) #:prefab)

;; Actions ⊃ Events
(struct spawn (boot) #:prefab)
(struct quit-network () #:prefab) ;; NB. An action. Compare (quit), a Transition.

;; A Behavior is a ((Option Event) Any -> Transition): a function
;; mapping an Event (or, in the #f case, a poll signal) and a
;; Process's current state to a Transition.
;;
;; A Transition is either
;;  - #f, a signal from a Process that it is inert and need not be
;;        scheduled until some Event relevant to it arrives; or,
;;  - a (transition Any (Constreeof Action)), a new Process state to
;;        be held by its Network and a sequence of Actions for the Network
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
(struct network (mux ;; Multiplexer
                 event-channel ;; incoming events
                 action-channel ;; outgoing actions
                 event-channels ;; (HashTable PID Channel) send events to children
                 action-channels ;; (HashTable PID Channel) actions coming from children
                 )
  #:transparent
  #:methods gen:prospect-pretty-printable
  [(define (prospect-pretty-print w [p (current-output-port)])
     (pretty-print-network w p))])

(struct network-boot-spec (actions) #:transparent)
                          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seals are used by protocols to prevent the routing tries from
;; examining internal structure of values.

(struct seal (contents)) ;; NB. Neither transparent nor prefab

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event? x) (or (patch? x) (message? x)))
(define (action? x) (or (event? x) (spawn? x) (quit-network? x)))
(define (internal-action? x) (or (equal? x 'quit) (action? x)))

(define (prepend-at-meta pattern level)
  (if (zero? level)
      pattern
      (at-meta (prepend-at-meta pattern (- level 1)))))

(define (observe-at-meta pattern level)
  (if (zero? level)
      (pattern->trie #t (observe pattern))
      (trie-union
       (pattern->trie #t (observe (prepend-at-meta pattern level)))
       (pattern->trie #t (at-meta (embedded-trie (observe-at-meta pattern (- level 1))))))))

(define (assert pattern #:meta-level [level 0])
  (patch (pattern->trie #t (prepend-at-meta pattern level)) (trie-empty)))
(define (retract pattern #:meta-level [level 0])
  (patch (trie-empty) (pattern->trie #t (prepend-at-meta pattern level))))

(define (sub pattern #:meta-level [level 0])
  (patch (observe-at-meta pattern level) (trie-empty)))
(define (unsub pattern #:meta-level [level 0])
  (patch (trie-empty) (observe-at-meta pattern level)))

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

;; Behavior Any (AsyncChannelOf Event) (AsyncChannelOf Action) -> Thread
;; Creates a thread running the behavior of a leaf actor.
;; Process incoming events and send any resulting actions.
(define (fork-leaf behavior state pid event-channel action-channel)
  (define (send-action a) (async-channel-put action-channel (cons pid a)))
  (define (send-actions as) (for ([a (in-list (flatten as))]) (send-action a)))
  (thread
   (lambda ()
     (let loop ([old-state state])
       (define event (async-channel-get event-channel))
       (begin
         (trace-process-step event pid behavior old-state)
         (invoke-process pid
                         (lambda () (clean-transition (ensure-transition (behavior event old-state))))
                         (match-lambda
                           [#f (loop old-state)]
                           [(and q (quit exn final-actions))
                            (trace-process-step-result event pid behavior old-state exn q)
                            (send-actions final-actions)
                            (send-action 'quit)]
                           [(and t (transition new-state new-actions))
                            (trace-process-step-result event pid behavior old-state #f t)
                            (send-actions new-actions)
                            (loop new-state)])
                         (lambda (exn)
                           (trace-process-step-result event pid behavior old-state exn #f)
                           (send-action 'quit))))))))

;; PID (Listof Action) (AsyncChannelOf Event) (AsyncChannelOf Action) -> Thread
(define (fork-network pid boot-actions event-channel action-channel)
  ;; Network PID Action (Network -> X) -> (U X #f)
  (define (action-step w src a k)
    (trace-internal-action src a w)
    (define wt1 (perform-action src a w))
    (trace-internal-action-result pid a w wt1)
    (match wt1
            [(? quit?) #f]
            [(transition new-network actions)
             (for ([a (in-list (flatten actions))])
               (async-channel-put action-channel (cons pid a)))
             (k new-network)]))
  (thread
   (lambda ()
     ;; run boot actions
     (define w0
       (for/fold ([w (network (mux)
                              event-channel
                              action-channel
                              (hash)
                              (hash))])
                 ([a (in-list boot-actions)])
         (let ([wn (action-step w 'meta a (lambda (x) x))])
           (or wn (kill-thread (current-thread))))))
     (let loop ([w w0])
       (define event-or-action
         (apply sync (cons (network-event-channel w)
                           (hash-values (network-action-channels w)))))
       (match event-or-action
         [(cons pid (? internal-action? action))
          (action-step w pid action loop)]
         [(? patch? delta)
          (action-step w 'meta (lift-patch delta) loop)]
         [(message body)
          (action-step w 'meta (message (at-meta body)) loop)])))))

;; Label -> Action -> Network -> (Transition Network)
(define (perform-action label a w)
  (match a
    [(spawn boot)
     ;; boot : -> (List Behavior Transition)
     (invoke-process 'booting
                     (lambda ()
                       (match (boot)
                         [(and results (? network-boot-spec?))
                          results]
                         [(and results (list (? procedure?) (? general-transition?)))
                          results]
                         [other
                          (error 'spawn
                                 "Spawn boot procedure must yield boot spec; received ~v"
                                 other)]))
                     (match-lambda
                       [(network-boot-spec boot-actions)
                        (transition (create-network w boot-actions) '())]
                       [(list behavior initial-transition)
                        (create-process w behavior initial-transition)])
                     (lambda (exn)
                       (log-error "Spawned process in network ~a died with exception:\n~a"
                                  (trace-pid-stack)
                                  (exn->string exn))
                       (transition w '())))]
    ['quit
     (define-values (new-mux _label delta delta-aggregate) (mux-remove-stream (network-mux w) label))
     (let ([w (disable-process w label #f)])
       ;; behavior & state in w already removed by disable-process
       (deliver-patches w new-mux label delta delta-aggregate))]
    [(? patch? delta-orig)
     (define-values (new-mux _label delta delta-aggregate)
       (mux-update-stream (network-mux w) label delta-orig))
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
                               [(pid (in-list (mux-route-message (network-mux w) body)))]
                       (send-event m pid w))
                     '()))]))

(define (empty-network)
  (network (mux)
          (make-async-channel)
          (make-async-channel)
          (hash)
          (hash)))

(module+ test
  (let* ([network (empty-network)]
         [t (perform-action 'meta (spawn-process (lambda (e s) #f) #f '()) network)])
    (check-true (transition? t))
    (match t
      [(transition new-network actions)
       (check-equal? (hash-count (network-event-channels new-network)) 1)
       (check-equal? (hash-count (network-action-channels new-network)) 1)
       (check-equal? actions (patch #f #f))])))

;; Network PID Behavior Any (Listof Action) -> Network
(define (boot-leaf w pid behavior initial-state boot-actions)
  (define event-chan (make-async-channel))
  (define action-chan (make-async-channel))
  (for ([a (in-list boot-actions)])
    (async-channel-put action-chan (cons pid a)))
  (fork-leaf behavior initial-state pid event-chan action-chan)
  (struct-copy network w
               [event-channels (hash-set (network-event-channels w) pid event-chan)]
               [action-channels (hash-set (network-action-channels w) pid action-chan)]))

;; Network (Listof Action) -> Network
(define (create-network w boot-actions)
  (define-values (new-mux pid delta delta-aggregate)
          (mux-add-stream (network-mux w) empty-patch))
  (define event-chan (make-async-channel))
  (define action-chan (make-async-channel))
  (fork-network pid boot-actions event-chan action-chan)
  (struct-copy network w
               [mux new-mux]
               [event-channels (hash-set (network-event-channels w) pid event-chan)]
               [action-channels (hash-set (network-action-channels w) pid action-chan)]))

;; Network Behavior (Transition Any) -> (Transition Network)
(define (create-process w behavior initial-transition)
  (if (not initial-transition)
      (transition w '()) ;; Uh, ok
      (let ()
        ;; postprocess : Network PID -> Network
        (define-values (postprocess initial-actions)
          (match (clean-transition initial-transition)
            [(and q (quit exn initial-actions0))
             (values (lambda (w pid remaining-actions)
                       (trace-process-step-result 'boot pid behavior (void) exn q)
                       (define chan (make-async-channel))
                       (for ([a (in-list remaining-actions)])
                         (async-channel-put chan (cons pid a)))
                       (struct-copy network w
                                    [action-channels (hash-set (network-action-channels w) pid chan)]))
                     (append initial-actions0 (list 'quit)))]
            [(and t (transition initial-state initial-actions0))
             (values (lambda (w pid remaining-actions)
                       (trace-process-step-result 'boot pid behavior (void) #f t)
                       (boot-leaf w pid behavior initial-state remaining-actions))
                     initial-actions0)]))
        ;; put the initial patch into affect to allow for a form of continuity
        ;; between spawned actors and their actions
        (define-values (initial-patch remaining-initial-actions)
          (match initial-actions
            [(cons (? patch? p) rest) (values p rest)]
            [other (values empty-patch other)]))
        (define-values (new-mux new-pid delta delta-aggregate)
          (mux-add-stream (network-mux w) initial-patch))
        (let ([w (postprocess w new-pid remaining-initial-actions)])
          (deliver-patches w new-mux new-pid delta delta-aggregate)))))

;; Event PID Network -> Network
(define (send-event e pid w)
  (define chan (hash-ref (network-event-channels w) pid #f))
  (when chan
    (async-channel-put chan e))
  w)

;; Patch PID Network -> Network
(define (send-event/guard delta pid w)
  (if (patch-empty? delta)
      w
      (send-event delta pid w)))

;; Network Mux Label Patch Patch -> (Transition Network)
(define (deliver-patches w new-mux acting-label delta delta-aggregate)
  (define-values (patches meta-action)
    (compute-patches (network-mux w) new-mux acting-label delta delta-aggregate))
  (transition (for/fold [(w (struct-copy network w [mux new-mux]))]
                        [(entry (in-list patches))]
                (match-define (cons label event) entry)
                (send-event/guard event label w))
              (if (action? meta-action)
                  meta-action
                  '())))

;; PID Exception Network -> Network
(define (disable-process cw pid exn)
  (when exn
    (log-error "Process ~a died with exception:\n~a"
               (cons pid (trace-pid-stack))
               (exn->string exn)))
  (struct-copy network cw
               [event-channels (hash-remove (network-event-channels cw) pid)]
               [action-channels (hash-remove (network-action-channels cw) pid)]))

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

(define (make-quit #:exception [exn #f] . actions)
  (quit exn actions))

(define-syntax spawn-process
  (syntax-rules ()
    [(_ #:name name-exp behavior-exp initial-state-exp initial-action-tree-exp)
     (spawn (lambda ()
              (list (let ((name name-exp)
                          (beh behavior-exp))
                      (if name (procedure-rename beh name) beh))
                    (transition initial-state-exp initial-action-tree-exp))))]
    [(_ behavior-exp initial-state-exp initial-action-tree-exp)
     (spawn (lambda ()
              (list behavior-exp
                    (transition initial-state-exp initial-action-tree-exp))))]))

(define-syntax-rule (spawn/stateless behavior-exp initial-action-tree-exp)
  (spawn-process (stateless-behavior-wrap behavior-exp)
                 (void)
                 initial-action-tree-exp))

(define ((stateless-behavior-wrap b) e state)
  (match (b e)
    [#f #f]
    [(? quit? q) q]
    [actions (transition state actions)]))

(define-syntax-rule (spawn-network boot-action ...)
  (make-spawn-network (lambda () (list boot-action ...))))

(define (make-spawn-network boot-actions-thunk)
  (spawn (lambda () (network-boot-spec (clean-actions (boot-actions-thunk))))))

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

(define (pretty-print-network w [p (current-output-port)])
  (match-define (network mux events-in actions-out event-chans action-chans) w)
  (fprintf p "NETWORK:\n")
  (fprintf p " - ~a live processes\n" (hash-count event-chans))
  (fprintf p " - ")
  (display (indented-port-output 3 (lambda (p) (prospect-pretty-print mux p)) #:first-line? #f) p)
  (for ([pid (set-union (hash-keys (mux-interest-table mux)) (hash-keys event-chans))])
    (fprintf p "      process ~a, CLAIMS:\n" pid )
    (display (indented-port-output 6 (lambda (p)
                                       (pretty-print-trie (mux-interests-of mux pid) p)))
             p)
    (newline p)))

