#lang racket/base

(provide ;; actor
         ;; network
         ;; background
         state

         until
         forever

         assert!
         retract!
         send!
         quit!

         ;; forall

         ;;----------------------------------------
         (struct-out actor-state)
         )

(require (for-syntax racket/base))
(require "support/dsl.rkt")

(define&provide-dsl-helper-syntaxes "state/until/forever form"
  [on
   assert
   track

   asserted
   retracted
   message
   rising-edge

   exists
   ])

(require (for-syntax racket/match))
(require (for-syntax racket/list))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/stx))

(require racket/set)
(require racket/match)

(require "core.rkt")
(require "route.rkt")
(require "endpoint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actor State

;; A Variables is a (Vectorof Any), storing the explicit and implicit
;; state variables of an actor, including tracked and implicit
;; aggregates.

;; A Script is a (Variables -> Variables). It is to be executed inside
;; the special syndicate-hll prompt, and so may have Instruction
;; side-effects.

;; An Instruction is one of
;; - (patch-instruction Patch (Void -> Instruction))
;; - (send-instruction Message (Void -> Instruction))
;; - (quit-instruction (Option Exn))
;; - (spawn-instruction LinkageKind Script (-> Variables) (Void -> Instruction))
;; - (script-complete-instruction Variables)
;; and represents a side-effect for an actor to take in its
;; interactions with the outside world.
;;
;; A LinkageKind is one of
;; - 'call, a blocking, exception-linked connection
;; - 'actor, a non-blocking, non-exception-linked connection
;; - 'network, a non-blocking, nested, non-exception-linked connection
;;
;; Patch Instructions are issued when the actor uses `assert!` and
;; `retract!`. Send instructions are issued when the actor uses
;; `send!`, and quit instructions when `quit!` is called.
;; Script-complete instructions are automatically issued when a Script
;; terminates successfully.
;;
;; Spawn instructions are issued when `actor`, `network`, and `state`
;; are used, directly or indirectly. (TODO: `background`?) The
;; included function yielding Variables sets up the initial variables
;; of the actor, and the included Script transforms the variables and
;; performs initial side effects, including establishing the ongoing
;; event handlers via add-endpoint actions. The included (Void ->
;; Instruction) continuation is released when the spawned actor
;; terminates (for blocking variants) or immediately following the
;; spawn (for non-blocking variants).
;;
;; (Background is done differently, with a new continuation for the
;; background script, and a self-send to activate it. (TODO))
;;
(struct patch-instruction (patch k) #:transparent)
(struct send-instruction (message k) #:transparent)
(struct quit-instruction (maybe-exn) #:transparent)
(struct spawn-instruction (linkage-kind init-script init-variables-fn k)
  #:transparent)
(struct script-complete-instruction (variables) #:transparent)

;; An ActorState is an (actor-state ... as below), describing the
;; state of an HLL actor.
;;
(struct actor-state (continuation-table ;; (Hashtable Symbol (Variables Any ... -> Instruction))
                     caller-id          ;; Symbol
                     self-id            ;; Symbol
                     variables          ;; Variables
                     pending-patch      ;; (Option Patch) - assert!/retract! patch being accumulated
                     )
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linkage protocol
;;
;; Linkages are used to both propagate values from callee to caller
;; and to monitor callee presence for exception-propagation.
;;   - assertion: LinkActive
;;   - message: LinkResult
;;   - role: Caller
;;       Monitors LinkActive to detect termination of the Callee,
;;       normal or abnormal. If LinkResult is received before
;;       LinkActive vanishes, termination was normal; otherwise, it
;;       was abnormal.
;;   - role: Callee
;;       Asserts LinkActive while it runs. Should send LinkResult
;;       before termination to indicate success and communicate values
;;       to Caller.
;;
;; A LinkActive is a (link-active Symbol Symbol), describing an
;; ongoing relationship between the indicated caller and callee.
(struct link-active (caller-id callee-id) #:prefab)
;;
;; A LinkResult is a (link-result Symbol Symbol (Listof Any)),
;; describing the final values yielded by a callee to its caller.
(struct link-result (caller-id callee-id values) #:prefab) ;; message

;; Projection for observing LinkActive.
(define link-active-projection (compile-projection (link-active ? (?!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing Instruction side-effects

(define prompt (make-continuation-prompt-tag 'syndicate-hll))

;; (Any ... -> Nothing) -> (Any ... -> Instruction)
(define (reply-to k)
  (lambda reply-values
    (call-with-continuation-prompt (lambda ()
                                     (with-handlers [((lambda (e) #t)
                                                      (lambda (exn)
                                                        (call-in-raw-context/abort
                                                         (lambda () (quit-instruction exn)))))]
                                       (apply k reply-values)
                                       (error 'reply-to "Script returned directly")))
                                   prompt)))

;; (-> Instruction) -> Nothing
(define (call-in-raw-context/abort proc)
  (abort-current-continuation prompt proc))

;; ((Any ... -> Instruction) -> Instruction)
(define (call-in-raw-context proc)
  (call-with-composable-continuation
   (lambda (k) (abort-current-continuation prompt (lambda () (proc (reply-to k)))))
   prompt))

;; Returns void
(define (assert! P)
  (call-in-raw-context
   (lambda (k) (patch-instruction (patch (pattern->matcher #t P) (matcher-empty)) k))))

;; Returns void
(define (retract! P)
  (call-in-raw-context
   (lambda (k) (patch-instruction (patch (matcher-empty) (pattern->matcher #t P)) k))))

;; Returns void
(define (send! M)
  (call-in-raw-context
   (lambda (k) (send-instruction M k))))

;; Does not return
(define (quit! [maybe-exn #f])
  (call-in-raw-context/abort
   (lambda () (quit-instruction maybe-exn))))

;; Returns new variables, plus values from spawned actor if any.
(define (spawn! linkage-kind init-script init-variables-fn)
  (call-in-raw-context
   (lambda (k) (spawn-instruction linkage-kind init-script init-variables-fn k))))

;; Syntax for spawning a 'call-linked actor.
(define-syntax (state stx)
  (syntax-parse stx
    [(_ #:init [I ...] [#:collect [(id init) ...] O ...] [E Oe ...] ...)
     (expand-state 'call #'(I ...) #'(id ...) #'(init ...) #'(O ...) #'([E Oe ...] ...))]
    [(_ [#:collect [(id init) ...] O ...] [E Oe ...] ...)
     (expand-state 'call #'() #'(id ...) #'(init ...) #'(O ...) #'([E Oe ...] ...))]
    [(_ #:init [I ...] [O ...] [E Oe ...] ...)
     (expand-state 'call #'(I ...) #'() #'() #'(O ...) #'([E Oe ...] ...))]
    [(_ [O ...] [E Oe ...] ...)
     (expand-state 'call #'() #'() #'() #'(O ...) #'([E Oe ...] ...))]))

;; Sugar
(define-syntax-rule (until E O ...)
  (state [O ...] [E (void)])) ;; TODO: return collected value(s)

;; Sugar
(define-syntax-rule (forever O ...)
  (state [O ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main behavior of HLL actors

;; Special EID used to track ad-hoc assertions
;; TODO: Revisit this, it is a bit ugly
(define *adhoc-eid* -2) ;; TODO: -1 is used in spawn-endpoint-group

;; Behavior
(define (generic-actor-behavior e s)
  (match e
    [(? patch/removed? p)
     (define continuation-table (actor-state-continuation-table s))
     (define quit?
       (for/or [(callee-id (matcher-project/set/single (patch-removed p) link-active-projection))]
         (hash-has-key? continuation-table callee-id)))
     (if quit? ;; TODO: raise exception instead? Signal the cause of the quit somehow?
         (quit)
         #f)]
    [(message (link-result _ callee-id reply-values))
     (invoke-stored-continuation s callee-id reply-values)]
    [_ #f]))

;; ActorState Symbol (Variables Any ... -> Instruction) -> ActorState
(define (store-continuation s callee-id get-next-instr)
  (struct-copy actor-state s
               [continuation-table
                (hash-set (actor-state-continuation-table s)
                          callee-id
                          get-next-instr)]))

;; ActorState Symbol (Listof Any) -> Transition
(define (invoke-stored-continuation s callee-id reply-values)
  (define continuation-table (actor-state-continuation-table s))
  (define continuation (hash-ref continuation-table callee-id #f))
  (define new-table (hash-remove continuation-table callee-id))
  (handle-actor-syscall (transition (struct-copy actor-state s [continuation-table new-table])
                                    '())
                        (apply continuation (actor-state-variables s) reply-values)))

;; ActorState Script -> Transition
(define (run-script s script)
  (handle-actor-syscall (transition s '())
                        ((reply-to (lambda (dummy)
                                     (define new-variables (script (actor-state-variables s)))
                                     (call-in-raw-context/abort
                                      (lambda ()
                                        (script-complete-instruction new-variables)))))
                         (void))))

;; Behavior -> Behavior
(define (compose-ongoing-handler ongoing-handler)
  (lambda (e s)
    (match (ongoing-handler e s)
      [#f (generic-actor-behavior e s)]
      [t (transition-bind (lambda (s) (generic-actor-behavior e s)) t)])))

;; Transition Instruction -> Transition
(define (handle-actor-syscall t instr)
  (match-define (transition s previous-actions) t)
  (match instr
    [(patch-instruction p get-next-instr)
     (define p0 (actor-state-pending-patch s))
     (handle-actor-syscall (transition (struct-copy actor-state s
                                                    [pending-patch (if p0 (patch-seq p0 p) p)])
                                       previous-actions)
                           (get-next-instr (void)))]
    [(send-instruction m get-next-instr)
     (handle-actor-syscall (transition (struct-copy actor-state s [pending-patch #f])
                                       (list previous-actions
                                             (as-endpoint *adhoc-eid* (actor-state-pending-patch s))
                                             (message m)))
                           (get-next-instr (void)))]
    [(quit-instruction maybe-exn)
     (quit #:exception maybe-exn
           (list previous-actions
                 (as-endpoint *adhoc-eid* (actor-state-pending-patch s))))]
    [(spawn-instruction linkage-kind init-script init-variables-fn get-next-instr)
     (define callee-id (gensym 'actor))
     (define callee-caller-id (and (eq? linkage-kind 'call) (actor-state-self-id s)))
     (define sub-to-callees
       (patch-seq (sub (link-active callee-id ?))
                  (sub (link-result callee-id ? ?))))
     (define initial-subs
       (if callee-caller-id
           (patch-seq sub-to-callees
                      (assert (link-active callee-caller-id callee-id)))
           sub-to-callees))
     (transition (store-continuation s callee-id get-next-instr)
                 (list previous-actions
                       (<spawn> (lambda ()
                                  ;; TODO: ↓ Handle quit, #f etc.
                                  (match-define (transition initial-state initial-actions)
                                    (transition-bind
                                     (lambda (s) (run-script s init-script))
                                     (transition (actor-state (hasheq)
                                                              callee-caller-id
                                                              callee-id
                                                              (init-variables-fn)
                                                              #f)
                                                 initial-subs)))
                                  (boot-endpoint-group initial-state initial-actions)))))]
    [(script-complete-instruction new-variables)
     (transition (struct-copy actor-state s
                              [pending-patch #f]
                              [variables new-variables])
                 (list previous-actions
                       (as-endpoint *adhoc-eid* (actor-state-pending-patch s))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation of HLL actors

(begin-for-syntax
  (define (expand-state linkage-kind init-actions binding-names binding-inits ongoings edges)
    (define state-variable-init-exps binding-inits)

    (define (allocate-state-variable! init-exp)
      (set! state-variable-init-exps
            (append state-variable-init-exps (list init-exp)))
      (- (length state-variable-init-exps) 1))

    (for ((edge (syntax->list edges)))
      (printf "~v\n" edge))

    #`(actor-state (hasheq)
                   (vector #,@state-variable-init-exps)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HLL pattern analysis

(begin-for-syntax
  (define (dollar-id? stx)
    (and (identifier? stx)
         (char=? (string-ref (symbol->string (syntax-e stx)) 0) #\$)))

  (define (undollar stx)
    (and (dollar-id? stx)
         (datum->syntax stx (string->symbol (substring (symbol->string (syntax-e stx)) 1)))))

  ;; Syntax -> (Values Projection AssertionSetPattern MatchPattern (ListOf Identifier))
  (define (analyze-pattern pat-stx)
    (syntax-case pat-stx ($ quasiquote unquote quote)
      ;; Extremely limited support for quasiquoting and quoting
      [(quasiquote (unquote p)) (analyze-pattern #'p)]
      [(quasiquote (p ...)) (analyze-pattern #'(list (quasiquote p) ...))]
      [(quasiquote p) (values #''p #''p #''p '())]
      [(quote p) (values #''p #''p #''p '())]

      [$v
       (dollar-id? #'$v)
       (with-syntax [(v (undollar #'$v))]
         (values #'(?!)
                 #'?
                 #'v
                 (list #'v)))]

      [($ v p)
       (let ()
         (define-values (pr g m bs) (analyze-pattern #'p))
         (when (not (null? bs))
           (raise-syntax-error #f "nested bindings not supported" pat-stx))
         (values #`(?! #,pr)
                 g
                 #`(and v #,m)
                 (list #'v)))]

      [(ctor p ...)
       (let ()
         (define parts (if (identifier? #'ctor) #'(p ...) #'(ctor p ...)))
         (define-values (pr g m bs)
           (for/fold [(pr '()) (g '()) (m '()) (bs '())] [(p (syntax->list parts))]
             (define-values (pr1 g1 m1 bs1) (analyze-pattern p))
             (values (cons pr1 pr)
                     (cons g1 g)
                     (cons m1 m)
                     (append bs1 bs))))
         (if (identifier? #'ctor)
             (values (cons #'ctor (reverse pr))
                     (cons #'ctor (reverse g))
                     (cons #'ctor (reverse m))
                     bs)
             (values (reverse pr)
                     (reverse g)
                     (reverse m)
                     bs)))]

      [non-pair
       (if (and (identifier? #'non-pair)
                (free-identifier=? #'non-pair #'_))
           (values #'?
                   #'?
                   #'_
                   '())
           (values #'non-pair
                   #'non-pair
                   #'(== non-pair)
                   '()))]))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require racket/pretty (for-syntax racket/pretty))

  (define (expand-and-print stx)
    (pretty-print (syntax->datum (expand stx))))

  (begin-for-syntax
    (define (analyze-and-print pat-stx)
      (let-values (((pr g m bs) (analyze-pattern pat-stx)))
        (pretty-print `((pr ,(map syntax->datum pr))
                        (g ,(map syntax->datum g))
                        (m ,(map syntax->datum m))
                        (bs ,(map syntax->datum bs))))))

    (analyze-and-print #'`(hello ,$who)))

  (expand-and-print
   #'(actor
      (until (= count 10)
             #:collect [(count 0)]
             (on (message `(hello ,$who))
                 (println "Got hello: ~a" who)
                 (+ count 1))))))
