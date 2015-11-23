#lang racket/base

;; TODO: syntax-id-rules: raise-syntax-error on set!, pointing users
;; to the #:update pseudo-action.

;; TODO: enforce presence of #:arguments, and enforce that it declares
;; all the free variables in the actor.

(provide ;; actor
         ;; network
         ;; background
         ;; state

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (state stx)
  (syntax-parse stx
    [(_ #:init [I ...] [#:collect [(id init) ...] O ...] [E Oe ...] ...)
     (expand-state 'state #'(I ...) #'(id ...) #'(init ...) #'(O ...) #'([E Oe ...] ...))]
    [(_ [#:collect [(id init) ...] O ...] [E Oe ...] ...)
     (expand-state 'state #'() #'(id ...) #'(init ...) #'(O ...) #'([E Oe ...] ...))]
    [(_ #:init [I ...] [O ...] [E Oe ...] ...)
     (expand-state 'state #'(I ...) #'() #'() #'(O ...) #'([E Oe ...] ...))]
    [(_ [O ...] [E Oe ...] ...)
     (expand-state 'state #'() #'() #'() #'(O ...) #'([E Oe ...] ...))]))

(define-syntax-rule (until E O ...)
  (state [O ...] [E (void)])) ;; TODO: return collected value(s)

(define-syntax-rule (forever O ...)
  (state [O ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct link-active (parent-id child-id) #:prefab) ;; assertion
(struct link (parent-id child-id values) #:prefab) ;; message

(define link-active-projection (compile-projection (link-active ? (?!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct actor-state (continuation-table ;; Own continuations
                     parent-id
                     self-id
                     variables ;; Explicit variables + tracked aggregates + implicit aggregates
                     adhoc-assertions ;; assert!/retract! aggregate
                     pending-patch ;; assert!/retract! patch being accumulated, or #f
                     )
  #:prefab)

(define (generic-actor-behaviour e s)
  (match e
    [(? patch/removed? p)
     (define continuation-table (actor-state-continuation-table s))
     (define quit?
       (for/or [(child-id (matcher-project/set/single (patch-removed p) link-active-projection))]
         (hash-has-key? continuation-table child-id)))
     (if quit? ;; TODO: raise exception instead? Signal the cause of the quit somehow?
         (quit)
         #f)]
    [(message (link _ child-id reply-values))
     (invoke-stored-continuation s child-id reply-values)]
    [_ #f]))

(define (store-continuation s child-id get-next-instr)
  (struct-copy actor-state s
               [continuation-table
                (hash-set (actor-state-continuation-table s)
                          child-id
                          get-next-instr)]))

(define (invoke-stored-continuation s child-id reply-values)
  (define continuation-table (actor-state-continuation-table s))
  (define continuation (hash-ref continuation-table child-id #f))
  (define new-table (hash-remove continuation-table child-id))
  (handle-actor-syscall (transition (struct-copy actor-state s [continuation-table new-table])
                                    '())
                        (apply continuation (actor-state-variables s) reply-values)))

(define (run-script s script)
  (handle-actor-syscall (transition s '())
                        ((reply-to (lambda (dummy)
                                     (call-with-values
                                      (lambda () (script (actor-state-variables s)))
                                      (lambda new-variables
                                        (call-in-raw-context/abort
                                         (lambda ()
                                           (script-complete-instruction new-variables)))))))
                         (void))))

(define (compose-ongoing-handler ongoing-handler)
  (lambda (e s)
    (match (ongoing-handler e s)
      [#f (generic-actor-behaviour e s)]
      [t (transition-bind (lambda (s) (generic-actor-behaviour e s)) t)])))

(define (handle-actor-syscall t instr)
  (match-define (transition s previous-actions) t)
  (match instr
    [(patch-instruction p get-next-instr)
     (define p0 (actor-state-pending-patch s))
     (handle-actor-syscall (transition (struct-copy actor-state s
                                        [adhoc-assertions
                                         (update-interests (actor-state-adhoc-assertions s) p)]
                                        [pending-patch
                                         (if p0 (patch-seq p0 p) p)])
                                       previous-actions)
                           (get-next-instr (void)))]
    [(send-instruction m get-next-instr)
     (handle-actor-syscall (transition (struct-copy actor-state s [pending-patch #f])
                                       (list previous-actions
                                             (actor-state-pending-patch s)
                                             (message m)))
                           (get-next-instr (void)))]
    [(quit-instruction maybe-exn)
     (quit #:exception maybe-exn
           (list previous-actions
                 (actor-state-pending-patch s)))]
    [(spawn-instruction actor-kind init-script init-variables-fn ongoing-handler get-next-instr)
     (define child-id (gensym 'actor))
     (define child-parent-id (and (eq? actor-kind 'state) (actor-state-self-id s)))
     (define sub-to-children
       (patch-seq (sub (link-active child-id ?))
                  (sub (link child-id ? ?))))
     (define initial-subs
       (if child-parent-id
           (patch-seq sub-to-children
                      (assert (link-active child-parent-id child-id)))
           sub-to-children))
     (transition (store-continuation s child-id get-next-instr)
                 (list previous-actions
                       (<spawn> (lambda ()
                                  (list (if ongoing-handler
                                            (compose-ongoing-handler ongoing-handler)
                                            generic-actor-behaviour)
                                        (transition-bind
                                         (lambda (s) (run-script s init-script))
                                         (transition (actor-state (hasheq)
                                                                  child-parent-id
                                                                  child-id
                                                                  (init-variables-fn)
                                                                  (matcher-empty)
                                                                  #f)
                                                     initial-subs)))))))]
    [(script-complete-instruction new-variables)
     (transition (struct-copy actor-state s
                              [pending-patch #f]
                              [variables new-variables])
                 (list previous-actions
                       (actor-state-pending-patch s)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prompt (make-continuation-prompt-tag 'syndicate-hll))

(define (reply-to k)
  (lambda reply-values
    (call-with-continuation-prompt (lambda ()
                                     (with-handlers [(values
                                                      (lambda (exn)
                                                        (call-in-raw-context/abort
                                                         (lambda () (quit-instruction exn)))))]
                                       (apply k reply-values)
                                       (error 'reply-to "Script returned directly")))
                                   prompt)))

(define (call-in-raw-context/abort proc)
  (abort-current-continuation prompt proc))

(define (call-in-raw-context proc)
  (call-with-composable-continuation
   (lambda (k) (abort-current-continuation prompt (lambda () (proc (reply-to k)))))
   prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct patch-instruction (patch k) #:transparent)
(struct send-instruction (message k) #:transparent)
(struct quit-instruction (maybe-exn) #:transparent)
(struct spawn-instruction (actor-kind init-script init-variables-fn ongoing-handler k) #:transparent)
(struct script-complete-instruction (variables) #:transparent)

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
(define (spawn! actor-kind init-script init-variables-fn ongoing-handler)
  (call-in-raw-context
   (lambda (k) (spawn-instruction actor-kind init-script init-variables-fn ongoing-handler k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Actor kinds:
;;  - 'actor
;;  - 'state
;;  - 'network
;;
;; Background is done differently, with a new continuation for the
;; background script, and a self-send to activate it.

(begin-for-syntax
  (define (expand-state actor-kind init-actions binding-names binding-inits ongoings edges)
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
