#lang racket/base
;; Core structures and utilities for implementation of Incremental Syndicate.

(provide (struct-out message)
         (except-out (struct-out quit) quit)
         (rename-out [quit <quit>])
         (except-out (struct-out actor) actor)
         (rename-out [actor <actor>])
         (struct-out quit-dataspace)
         (struct-out transition)

         (struct-out process)

         (struct-out seal)
         sealof

         (all-from-out "patch.rkt")

         ;; imported from trie.rkt:
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
         projection-arity
         trie-project
         trie-project/set
         trie-project/set/single
         project-assertions

         event?
         action?
         match-event

         meta-label?

         assert
         retract
         sub
         unsub

         (rename-out [make-quit quit])
         make-actor
         (rename-out [boot-process actor])
         actor/stateless

         general-transition?
         ensure-transition

         transition-bind
         sequence-transitions
         sequence-transitions*
         sequence-transitions0
         sequence-transitions0*

         clean-actions
         clean-transition

         update-process-state
         actor->process+transition)

(require racket/match)
(require (only-in racket/list flatten))
(require "trie.rkt")
(require "patch.rkt")
(require "mux.rkt")

;; Events = Patches ∪ Messages
(struct message (body) #:prefab)

;; Actions ⊃ Events
(struct actor (boot) #:prefab)
(struct quit-dataspace () #:prefab) ;; NB. An action. Compare (quit), a Transition.

;; A Behavior is a ((Option Event) Any -> Transition): a function
;; mapping an Event (or, in the #f case, a poll signal) and a
;; Process's current state to a Transition.
;;
;; A Transition is either
;;  - #f, a signal from a Process that it is inert and need not be
;;        scheduled until some Event relevant to it arrives; or,
;;  - a (transition Any (Constreeof Action)), a new Process state to
;;        be held by its Dataspace and a sequence of Actions for the Dataspace
;;        to take on the transitioning Process's behalf.
;;  - a (quit (Option Exn) (Constreeof Action)), signalling that the
;;        Process should never again be handed an event, and that any
;;        queued actions should be performed, followed by the sequence
;;        of Actions given, and then the process should be
;;        garbage-collected. The optional Exn is only used for
;;        debugging purposes; #f means normal termination.
(struct transition (state actions) #:transparent)
(struct quit (exn actions) #:prefab)

;; A Process is per-process data: (process Any Behavior Any)
(struct process (name behavior state) #:transparent)

;; A PID is a Nat.
;; A Label is a PID or 'meta.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seals are used by protocols to prevent the routing tries from
;; examining internal structure of values.

(struct seal (contents)) ;; NB. Neither transparent nor prefab

;; contract -> contract
(define ((sealof c) x)
  (and (seal? x) (c (seal-contents x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event? x) (or (patch? x) (message? x)))
(define (action? x) (or (event? x) (actor? x) (quit-dataspace? x)))

(define-syntax-rule (match-event e clause ...)
  (match e
    clause ...
    [_ #f]))

(define (assert pattern)
  (patch (pattern->trie '<assert> pattern) trie-empty))
(define (retract pattern)
  (patch trie-empty (pattern->trie '<retract> pattern)))

(define (sub pattern)
  (patch (pattern->trie '<sub> (observe pattern)) trie-empty))
(define (unsub pattern)
  (patch trie-empty (pattern->trie '<unsub> (observe pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (general-transition? v)
  (or (not v) (transition? v) (quit? v) (void? v)))

(define (ensure-transition v)
  (if (general-transition? v)
      v
      (raise (exn:fail:contract (format "Expected transition, quit, #f or (void); got ~v" v)
				(current-continuation-marks)))))

(define (clean-transition t)
  (match t
    [#f #f]
    [(quit exn actions) (quit exn (clean-actions actions))]
    [(transition state actions) (transition state (clean-actions actions))]
    [(? void?) #f]))

(define (clean-actions actions)
  (filter (lambda (x) (and (action? x) (not (patch-empty? x)))) (flatten actions)))

(define (update-process-state i new-state)
  (struct-copy process i [state new-state]))

(define (actor->process+transition s)
  (match-define (list beh t name) ((actor-boot s)))
  (values (process name beh 'undefined-initial-state) t))

(define (make-quit #:exception [exn #f] . actions)
  (quit exn actions))

(define (make-actor actor-producing-thunk)
  (actor (let ((parameterization (current-parameterization)))
           (lambda ()
             (call-with-parameterization
              parameterization
              (lambda ()
                (match (actor-producing-thunk)
                  [(list (? procedure? raw-beh) (? general-transition? txn) name)
                   (list (lambda (e s)
                           (call-with-parameterization parameterization (lambda () (raw-beh e s))))
                         txn
                         name)]
                  [other other]))))))) ;; punt on error checking to dataspace boot code

(define-syntax boot-process
  (syntax-rules ()
    [(_ #:name name-exp behavior-exp initial-state-exp initial-action-tree-exp)
     (make-actor (lambda ()
                   (list behavior-exp
                         (transition initial-state-exp initial-action-tree-exp)
                         name-exp)))]
    [(_ behavior-exp initial-state-exp initial-action-tree-exp)
     (make-actor (lambda ()
                   (list behavior-exp
                         (transition initial-state-exp initial-action-tree-exp)
                         #f)))]))

(define-syntax actor/stateless
  (syntax-rules ()
    [(_ #:name name-exp behavior-exp initial-action-tree-exp)
     (boot-process #:name name-exp
                    (stateless-behavior-wrap behavior-exp)
                    (void)
                    initial-action-tree-exp)]
    [(_ behavior-exp initial-action-tree-exp)
     (boot-process (stateless-behavior-wrap behavior-exp)
                    (void)
                    initial-action-tree-exp)]))

(define ((stateless-behavior-wrap b) e state)
  (match (b e)
    [#f #f]
    [(? quit? q) q]
    [actions (transition state actions)]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; eval: (put 'match-event 'scheme-indent-function 1)
;;; eval: (put 'match-event 'racket-indent-function 1)
;;; End:
