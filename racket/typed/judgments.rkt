#lang turnstile

(provide (for-syntax (all-defined-out)))

(require "base-types.rkt")
(require "user-ctors.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple Judgments on Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→ τ ...) #f]
    [(~Actor τ) #f]
    [_ #t]))

;; Flattish-Type -> Bool
(define-for-syntax (finite? t)
  (syntax-parse t
    [~★/t #f]
    [(~U* τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~Tuple τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~constructor-type _ τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~Observe τ:type)
     (finite? #'τ)]
    [(~Inbound τ:type)
     (finite? #'τ)]
    [(~Outbound τ:type)
     (finite? #'τ)]
    ;; TODO - this would introduce a circular dependency. I think Turnstile has a catch-all type
    ;; pattern expander I could use here instead.
    #;[(~Set τ:type)
     (finite? #'τ)]
    [(~Message τ:type)
     (finite? #'τ)]
    [_ #t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type Type -> Bool
(define-for-syntax (<: t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [((~U* τ1 ...) _)
     (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
    [((~Actor τ1) (~Actor τ2))
     (and (<: #'τ1 #'τ2)
          (<: (∩ (strip-? #'τ1) #'τ2) #'τ1))]
    [((~AssertionSet τ1) (~AssertionSet τ2))
     (<: #'τ1 #'τ2)]
    #;[((~Set τ1) (~Set τ2))
     (<: #'τ1 #'τ2)]
    [((~Patch τ11 τ12) (~Patch τ21 τ22))
     (and (<: #'τ11 #'τ21)
          (<: #'τ12 #'τ22))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
     (stx-andmap <: #'(τ1 ...) #'(τ2 ...))]
    [(_ ~★/t)
     (flat-type? t1)]
    [((~Observe τ1:type) (~Observe τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Message τ1:type) (~Message τ2:type))
     (<: #'τ1 #'τ2)]
    [((~constructor-type t1 τ1:type ...) (~constructor-type t2 τ2:type ...))
     #:when (tags-equal? #'t1 #'t2)
     (and (stx-length=? #'(τ1 ...) #'(τ2 ...))
          (stx-andmap <: #'(τ1 ...) #'(τ2 ...)))]
    [((~→ τ-in1 ... τ-out1) (~→ τ-in2 ... τ-out2))
     #:when (stx-length=? #'(τ-in1 ...) #'(τ-in2 ...))
     (and (stx-andmap <: #'(τ-in2 ...) #'(τ-in1 ...))
          (<: #'τ-out1 #'τ-out2))]
    [(~Discard _)
     #t]
    ;; TODO: clauses for Roles, and so on
    ;; should probably put this first.
    [_ (type=? t1 t2)]))

;; shortcuts for mapping
(define-for-syntax ((<:l l) r)
  (<: l r))

(define-for-syntax ((<:r r) l)
  (<: l r))

;; Flat-Type Flat-Type -> Type
(define-for-syntax (∩ t1 t2)
  (unless (and (flat-type? t1) (flat-type? t2))
    (error '∩ "expected two flat-types"))
  (syntax-parse #`(#,t1 #,t2)
    [(_ ~★/t)
     t1]
    [(~★/t _)
     t2]
    [(_ _)
     #:when (type=? t1 t2)
     t1]
    [((~U* τ1:type ...) _)
     ((current-type-eval) #`(U #,@(stx-map (lambda (t) (∩ t t2)) #'(τ1 ...))))]
    [(_ (~U* τ2:type ...))
     ((current-type-eval) #`(U #,@(stx-map (lambda (t) (∩ t1 t)) #'(τ2 ...))))]
    [((~AssertionSet τ1) (~AssertionSet τ2))
     #:with τ12 (∩ #'τ1 #'τ2)
     ((current-type-eval) #'(AssertionSet τ12))]
    #;[((~Set τ1) (~Set τ2))
     #:with τ12 (∩ #'τ1 #'τ2)
     ((current-type-eval) #'(Set τ12))]
    [((~Patch τ11 τ12) (~Patch τ21 τ22))
     #:with τ1 (∩ #'τ11 #'τ12)
     #:with τ2 (∩ #'τ21 #'τ22)
     ((current-type-eval) #'(Patch τ1 τ2))]
    ;; all of these fail-when/unless clauses are meant to cause this through to
    ;; the last case and result in ⊥.
    ;; Also, using <: is OK, even though <: refers to ∩, because <:'s use of ∩ is only
    ;; in the Actor case.
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:fail-unless (stx-length=? #'(τ1 ...) #'(τ2 ...)) #f
     #:with (τ ...) (stx-map ∩ #'(τ1 ...) #'(τ2 ...))
     ;; I don't think stx-ormap is part of the documented api of turnstile *shrug*
     #:fail-when (stx-ormap (lambda (t) (<: t ((current-type-eval) #'(U)))) #'(τ ...)) #f
     ((current-type-eval) #'(Tuple τ ...))]
    [((~constructor-type tag1 τ1:type ...) (~constructor-type tag2 τ2:type ...))
     #:when (tags-equal? #'tag1 #'tag2)
     #:with (τ ...) (stx-map ∩ #'(τ1 ...) #'(τ2 ...))
     #:fail-when (stx-ormap (lambda (t) (<: t ((current-type-eval) #'(U)))) #'(τ ...)) #f
     (make-cons-type t1 #'(τ ...))]
    ;; these three are just the same :(
    [((~Observe τ1:type) (~Observe τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ ((current-type-eval) #'(U))) #f
     ((current-type-eval) #'(Observe τ))]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ ((current-type-eval) #'(U))) #f
     ((current-type-eval) #'(Inbound τ))]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ ((current-type-eval) #'(U))) #f
     ((current-type-eval) #'(Outbound τ))]
    [((~Message τ1:type) (~Message τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ ((current-type-eval) #'(U))) #f
     ((current-type-eval) #'(Message τ))]
    [_ ((current-type-eval) #'(U))]))

;; Type Type -> Bool
;; first type is the contents of the set/dataspace
;; second type is the type of a pattern
(define-for-syntax (project-safe? t1 t2)
  ;; TODO - messages
  (syntax-parse #`(#,t1 #,t2)
    [(_ (~Bind τ2:type))
     (and (finite? t1) (<: t1 #'τ2))]
    [(_ ~Discard)
     #t]
    [(_ ~★/t)
     #t]
    [((~U* τ1:type ...) _)
     (stx-andmap (lambda (t) (project-safe? t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-andmap (lambda (t) (project-safe? t1 t)) #'(τ2 ...))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (overlap? t1 t2)
     (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
    [((~constructor-type _ τ1:type ...) (~constructor-type _ τ2:type ...))
     #:when (overlap? t1 t2)
     (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
    [((~Observe τ1:type) (~Observe τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Message τ1:type) (~Message τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [_ #t]))

;; AssertionType PatternType -> Bool
;; Is it possible for things of these two types to match each other?
;; Flattish-Type = Flat-Types + ★/t, Bind, Discard (assertion and pattern types)
(define-for-syntax (overlap? t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [(~★/t _) #t]
    [(_ (~Bind _)) #t]
    [(_ ~Discard) #t]
    [(_ ~★/t) #t]
    [((~U* τ1:type ...) _)
     (stx-ormap (lambda (t) (overlap? t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-ormap (lambda (t) (overlap? t1 t)) #'(τ2 ...))]
    #;[((~List _) (~List _))
     ;; share the empty list
     #t]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     (and (stx-length=? #'(τ1 ...) #'(τ2 ...))
          (stx-andmap overlap? #'(τ1 ...) #'(τ2 ...)))]
    [((~constructor-type t1 τ1:type ...) (~constructor-type t2 τ2:type ...))
     (and (tags-equal? #'t1 #'t2)
          (stx-andmap overlap? #'(τ1 ...) #'(τ2 ...)))]
    [((~Observe τ1:type) (~Observe τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Message τ1:type) (~Message τ2:type))
     (overlap? #'τ1 #'τ2)]
    [_ (<: t1 t2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Related Metafunctions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (strip-? t)
  ((current-type-eval)
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     ;; since (Observe X) can match (Message X):
     ;; doing this specifically for the intersection operation in the spawn rule, need to check other
     ;; uses
     [(~Observe τ) #'(U τ (Message τ))]
     [_ #'(U*)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Role Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RoleType RoleType -> Bool
;; Check that role r implements role spec (possibly does more)
(define-for-syntax (role-implements? r spec)
  (syntax-parse #`(#,r #,spec)
    ;; TODO: cases for unions, stop
    [((~Role (x:id) (~or (~Shares τ-s1) (~Sends τ-m1) (~Reacts τ-if1 τ-then1 ...)) ...)
      (~Role (y:id) (~or (~Shares τ-s2) (~Sends τ-m2) (~Reacts τ-if2 τ-then2 ...)) ...))
     #:when (free-identifier=? #'x #'y)
     (and
      ;; for each assertion in the spec, there must be a suitable assertion in the actual
      ;; TODO: this kinda ignores numerosity, can one assertion in r cover multiple assertions in spec?
      (for/and [(s2 (in-syntax #'(τ-s2 ...)))]
        (stx-ormap (<:l s2) #'(τ-s1 ...)))
      ;; similar for messages
      (for/and [(m2 (in-syntax #'(τ-m2 ...)))]
        (stx-ormap (<:l m2) #'(τ-m1 ...)))
      (for/and [(s2 (in-syntax #'((τ-if2 (τ-then2 ...)) ...)))]
        (define/syntax-parse (τ-if2 (τ-then2 ...)) s2)
        (for/or [(s1 (in-syntax #'((τ-if1 (τ-then1 ...)) ...)))]
          (define/syntax-parse (τ-if1 (τ-then1 ...)) s1)
          ;; the event descriptors need to line up
          (and (condition-covers? #'τ-if1 #'τ-if2)
               ;; and for each specified response to the event, there needs to be a similar one in the
               ;; the actual
               (stx-andmap (lambda (s) (stx-ormap (lambda (r) (role-implements? r s)) #'(τ-then1 ...)))
                           #'(τ-then2 ...))))))]
    [((~Role (x:id) _ ...)
      (~Role (y:id) _ ...))
     (role-implements? (subst #'y #'x r) spec)]
    [((~Stop x:id τ1 ...)
      (~Stop y:id τ2 ...))
     (and
      (free-identifier=? #'x #'y)
      (for/and ([t2 (in-syntax #'(τ2 ...))])
        (for/or ([t1 (in-syntax #'(τ1 ...))])
          (role-implements? t1 t2))))]
    ;; seems like this check might be in the wrong place
    [((~Sends τ-m1)
      (~Sends τ-m2))
     (<: #'τ-m1 #'τ-m2)]
    [((~Actor _)
      (~Actor _))
     ;; spawned actor OK in specified dataspace
     (<: r spec)]))

;; it's ok for x to respond to strictly more events than y
(define-for-syntax (condition-covers? x y)
  (or
   ;; covers Start,Stop,Dataflow
   (type=? x y)
   (syntax-parse #`(#,x #,y)
     [((~Know τ1) (~Know τ2))
      (<: (pattern-matching-assertions #'τ2)
          (pattern-matching-assertions #'τ1))]
     [((~¬Know τ1) (~¬Know τ2))
      (<: (pattern-matching-assertions #'τ2)
          (pattern-matching-assertions #'τ1))]
     [((~Message τ1) (~Message τ2))
      (<: (pattern-matching-assertions #'τ2)
          (pattern-matching-assertions #'τ1))]
     [_ #f])))

;; PatternType -> Type
(define-for-syntax (pattern-matching-assertions t)
  (syntax-parse t
    [(~Bind τ)
     #'τ]
    [~Discard
     ((current-type-eval) #'★/t)]
    [(~U* τ ...)
     ((current-type-eval) #`(U #,@(stx-map pattern-matching-assertions #'(τ ...))))]
    [(~Tuple τ ...)
     ((current-type-eval) #`(Tuple #,@(stx-map pattern-matching-assertions #'(τ ...))))]
    [(~Observe τ)
     ((current-type-eval) #`(Observe #,(pattern-matching-assertions #'τ)))]
    [(~Inbound τ)
     ((current-type-eval) #`(Inbound #,(pattern-matching-assertions #'τ)))]
    [(~Outbound τ)
     ((current-type-eval) #`(Outbound #,(pattern-matching-assertions #'τ)))]
    [(~Message τ)
     ((current-type-eval) #`(Message #,(pattern-matching-assertions #'τ)))]
    [(~constructor-type _ τ ...)
     (make-cons-type t (stx-map pattern-matching-assertions #'(τ ...)))]
    [_ t]))

(module+ test
  (displayln "skipping commented for-syntax tests because it's slow")
  #;(begin-for-syntax
    ;; TESTS
    (let ()
      ;; utils
      (local-require syntax/parse/define
                     rackunit)
      (define te (current-type-eval))
      (define-syntax-parser check-role-implements?
        [(_ r1 r2)
         (quasisyntax/loc this-syntax
           (check-true (role-implements? (te #'r1) (te #'r2))))])
      (define-syntax-parser check-role-not-implements?
        [(_ r1 r2)
         (quasisyntax/loc this-syntax
           (check-false (role-implements? (te #'r1) (te #'r2))))])
      ;; Name Related
      (check-role-implements? (Role (x)) (Role (x)))
      (check-role-implements? (Role (x)) (Role (y)))
      ;; Assertion Related
      (check-role-not-implements? (Role (x)) (Role (y) (Shares Int)))
      (check-role-implements? (Role (x) (Shares Int)) (Role (y)))
      (check-role-implements? (Role (x) (Shares Int)) (Role (y) (Shares Int)))
      (check-role-implements? (Role (x)
                                    (Shares Int)
                                    (Shares String))
                              (Role (y)
                                    (Shares Int)
                                    (Shares String)))
      (check-role-implements? (Role (x)
                                    (Shares String)
                                    (Shares Int))
                              (Role (y)
                                    (Shares Int)
                                    (Shares String)))
      (check-role-not-implements? (Role (x)
                                        (Shares Int))
                                  (Role (y)
                                        (Shares Int)
                                        (Shares String)))
      ;; Reactions
      (check-role-implements? (Role (x)
                                    (Reacts (Know Int)))
                              (Role (y)
                                    (Reacts (Know Int))))
      (check-role-implements? (Role (x)
                                    (Reacts (Know Int))
                                    (Shares String))
                              (Role (y)
                                    (Reacts (Know Int))))
      (check-role-implements? (Role (x)
                                    (Reacts (Know Int)
                                            (Role (y) (Shares String))))
                              (Role (y)
                                    (Reacts (Know Int))))
      (check-role-not-implements? (Role (x))
                                  (Role (y)
                                        (Reacts (Know Int))))
      (check-role-not-implements? (Role (x)
                                        (Reacts (Know String)))
                                  (Role (y)
                                        (Reacts (Know Int))))
      ;; these two might need to be reconsidered
      (check-role-not-implements? (Role (x)
                                        (Shares (Observe ★/t)))
                                  (Role (y)
                                        (Reacts (Know Int))))
      (check-role-not-implements? (Role (x)
                                        (Shares (Observe Int)))
                                  (Role (y)
                                        (Reacts (Know Int))))
      (check-role-implements? (Role (x)
                                    (Reacts (Know Int)
                                            (Role (x2) (Shares String))))
                              (Role (y)
                                    (Reacts (Know Int)
                                            (Role (y2) (Shares String)))))
      (check-role-implements? (Role (x)
                                    (Reacts (¬Know Int)
                                            (Role (x2) (Shares String))))
                              (Role (y)
                                    (Reacts (¬Know Int)
                                            (Role (y2) (Shares String)))))
      (check-role-implements? (Role (x)
                                    (Reacts OnStart
                                            (Role (x2) (Shares String))))
                              (Role (y)
                                    (Reacts OnStart
                                            (Role (y2) (Shares String)))))
      (check-role-implements? (Role (x)
                                    (Reacts OnStop
                                            (Role (x2) (Shares String))))
                              (Role (y)
                                    (Reacts OnStop
                                            (Role (y2) (Shares String)))))
      (check-role-implements? (Role (x)
                                    (Reacts OnDataflow
                                            (Role (x2) (Shares String))))
                              (Role (y)
                                    (Reacts OnDataflow
                                            (Role (y2) (Shares String)))))
      (check-role-not-implements? (Role (x)
                                        (Reacts (Know Int)
                                                (Role (x2) (Shares String))))
                                  (Role (y)
                                        (Reacts (Know Int)
                                                (Role (y2) (Shares String))
                                                (Role (y3) (Shares Int)))))
      (check-role-implements? (Role (x)
                                    (Reacts (Know Int)
                                            (Role (x3) (Shares Int))
                                            (Role (x2) (Shares String))))
                              (Role (y)
                                    (Reacts (Know Int)
                                            (Role (y2) (Shares String))
                                            (Role (y3) (Shares Int)))))
      ;; also not sure about this one
      (check-role-implements? (Role (x)
                                    (Reacts (Know Int)
                                            (Role (x2)
                                                  (Shares String)
                                                  (Shares Int))))
                              (Role (y)
                                    (Reacts (Know Int)
                                            (Role (y2) (Shares String))
                                            (Role (y3) (Shares Int)))))
      ;; Stop
      ;; these all error when trying to create the Stop type :<
#|
      (check-role-implements? (Role (x)
                                    (Reacts OnStart (Stop x)))
                              (Role (x)
                                    (Reacts OnStart (Stop x))))
      (check-role-implements? (Role (x)
                                    (Reacts OnStart (Stop x)))
                              (Role (y)
                                    (Reacts OnStart (Stop y))))
      (check-role-implements? (Role (x)
                                    (Reacts OnStart (Stop x (Role (x2) (Shares Int)))))
                              (Role (y)
                                    (Reacts OnStart (Stop y) (Role (y2) (Shares Int)))))
      (check-role-not-implements? (Role (x)
                                        (Reacts OnStart (Stop x (Role (x2) (Shares String)))))
                                  (Role (y)
                                        (Reacts OnStart (Stop y) (Role (y2) (Shares Int)))))
      (check-role-not-implements? (Role (x)
                                        (Reacts OnStart))
                                  (Role (y)
                                        (Reacts OnStart (Stop y) (Role (y2) (Shares Int)))))
|#
      ;; Spawning Actors
      (check-role-implements? (Role (x)
                                    (Reacts OnStart (Actor Int)))
                              (Role (x)
                                    (Reacts OnStart (Actor Int))))
      (check-role-implements? (Role (x)
                                    (Reacts OnStart (Actor Int)))
                              (Role (x)
                                    (Reacts OnStart (Actor (U Int String)))))
      (check-role-not-implements? (Role (x)
                                    (Reacts OnStart (Actor Bool)))
                              (Role (x)
                                    (Reacts OnStart (Actor (U Int String)))))
      )))