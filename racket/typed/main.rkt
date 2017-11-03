#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         #%app
         #%top-interaction
         require only-in
         ;; Types
         Int Bool String Tuple Bind Discard Case → Facet FacetName Field ★
         Observe Inbound Outbound Actor U
         ;; Statements
         let-field let-function spawn dataspace facet set! begin stop unsafe-do
         ;; endpoints
         assert on
         ;; expressions
         tuple λ ref observe inbound outbound
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         + - displayln
         ;; DEBUG
         (rename-out [printf- printf])
         )

(require (rename-in racket/match [match-lambda match-lambda-]))
(require (prefix-in syndicate: syndicate/actor-lang))

(module+ test
  (require rackunit)
  (require turnstile/rackunit-typechecking))

;(require syndicate/actor-lang)
#;(provide (all-from-out syndicate/actor-lang))

(define-base-types Int Bool String Discard ★ FacetName)
(define-type-constructor Field #:arity = 1)
(define-type-constructor Facet #:arity = 3)
(define-type-constructor Bind #:arity = 1)
(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor U #:arity >= 0)
(define-type-constructor Case #:arity >= 0)
(define-type-constructor → #:arity > 0)
(define-type-constructor Observe #:arity = 1)
(define-type-constructor Inbound #:arity = 1)
(define-type-constructor Outbound #:arity = 1)
(define-type-constructor Actor #:arity = 1)

(begin-for-syntax
  (define-syntax-class exp
    #:datum-literals (tuple λ ref)
    (pattern (~or (o:prim-op es:exp ...)
                  basic-val
                  (k:kons1 e:exp)
                  (tuple es:exp ...)
                  (ref x:id)
                  (λ [p:pat s:stmt] ...))))

  ;; constructors with arity one
  (define-syntax-class kons1
    (pattern (~or (~literal observe)
                  (~literal inbound)
                  (~literal outbound))))

  (define-syntax-class basic-val
    (pattern (~or boolean
                  integer
                  string)))

  (define-syntax-class prim-op
    (pattern (~or (~literal +)
                  (~literal -)
                  (~literal displayln))))

  (define-syntax-class stmt
    #:datum-literals (:
                      begin
                      let-field
                      let-function
                      set!
                      spawn
                      dataspace
                      stop
                      facet
                      unsafe-do)
    (pattern (~or (begin seq:stmt ...)
                  (e1:exp e2:exp)
                  (let-field [x:id : τ:type e:exp] lf-body:stmt)
                  (let-function [f:id : τ:type e:exp] let-fun-body:stmt)
                  (set! x:id e:exp)
                  (spawn τ:type s:stmt)
                  (dataspace τ:type nested:stmt ...)
                  (stop x:id s:stmt)
                  (facet x:id ep:endpoint ...)
                  ;; note racket expr, not exp
                  (unsafe-do rkt:expr ...))))

  (define-syntax-class endpoint
    #:datum-literals (on start stop)
    (pattern (~or (on ed:event-desc s:stmt)
                  (assert e:exp))))

  (define-syntax-class event-desc
    #:datum-literals (start stop asserted retracted)
    (pattern (~or start
                  stop
                  (asserted p:pat)
                  (retracted p:pat))))
                  
  (define-syntax-class pat
    #:datum-literals (tuple _ discard bind)
    #:attributes (syndicate-pattern match-pattern)
    (pattern (~or (~and (tuple ps:pat ...)
                        (~bind [syndicate-pattern #'(list 'tuple ps.syndicate-pattern ...)]
                               [match-pattern #'(list 'tuple ps.match-pattern ...)]))
                  (~and (k:kons1 p:pat)
                        ;; not sure if this gets the binding of k right
                        (~bind [syndicate-pattern #'(k p.syndicate-pattern)]
                               [match-pattern #'(k p.match-pattern)]))
                  (~and (bind ~! x:id τ:type)
                        (~bind [syndicate-pattern #'($ x)]
                               [match-pattern #'x]))
                  (~and discard
                        (~bind [syndicate-pattern #'_]
                               [match-pattern #'_]))
                  (~and x:id
                        (~bind [syndicate-pattern #'x]
                               [match-pattern #'(== x)]))
                  (~and e:exp
                        (~bind [syndicate-pattern #'e]
                               [match-pattern #'(== e)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

;; Bind Discard Case → Facet Field

;; TODO: subtyping for procedures, facets, fields

;; Type Type -> Bool
(define-for-syntax (<: t1 t2)
  #;(printf "Checking ~a <: ~a\n" (type->str t1) (type->str t2))
  ;; should add a check for type=?
  (syntax-parse #`(#,t1 #,t2)
    #;[(τ1 τ2) #:do [(displayln (type->str #'τ1))
                   (displayln (type->str #'τ2))]
             #:when #f
             (error "")]
    [((~U τ1 ...) _)
     #;(printf "union on the left\n")
     (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
    [(_ (~U τ2:type ...))
     (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
    [((~Actor τ1:type) (~Actor τ2:type))
     (and (<: #'τ1 #'τ2)
          (<: ((current-type-eval) (∩ (strip-? #'τ1) #'τ2)) #'τ1))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
     (stx-andmap <: #'(τ1 ...) #'(τ2 ...))]
    [(_ ~★)
     (flat-type? t1)]
    [((~Observe τ1:type) (~Observe τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (<: #'τ1 #'τ2)]
    ;; should probably put this first.
    [_ (type=? t1 t2)]))

;; Flat-Type Flat-Type -> Type
(define-for-syntax (∩ t1 t2)
  (unless (and (flat-type? t1) (flat-type? t2))
    (error '∩ "expected two flat-types"))
  (syntax-parse #`(#,t1 #,t2)
    [(_ ~★)
     t1]
    [(~★ _)
     t2]
    [(_ _)
     #:when (type=? t1 t2)
     t1]
    [((~U τ1:type ...) _)
     #`(U #,@(stx-map (lambda (t) (∩ t t2)) #'(τ1 ...)))]
    [(_ (~U τ2:type ...))
     #`(U #,@(stx-map (lambda (t) (∩ t1 t)) #'(τ2 ...)))]
    ;; all of these fail-when/unless clauses are meant to cause this through to
    ;; the last case and result in ⊥.
    ;; Also, using <: is OK, even though <: refers to ∩, because <:'s use of ∩ is only
    ;; in the Actor case.
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:fail-unless (stx-length=? #'(τ1 ...) #'(τ2 ...)) #f
     #:with (τ ...) (stx-map ∩ #'(τ1 ...) #'(τ2 ...))
     ;; I don't think stx-ormap is part of the documented api of turnstile *shrug*
     #:fail-when (stx-ormap (lambda (t) (<: t #'(U))) #'(τ ...)) #f
     #'(Tuple τ ...)]
    ;; these three are just the same :(
    [((~Observe τ1:type) (~Observe τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ #'(U)) #f
     #'(Observe τ)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ #'(U)) #f
     #'(Inbound τ)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ #'(U)) #f
     #'(Outbound τ)]
    [_ #'(U)]))

;; Type Type -> Bool
;; first type is the contents of the set
;; second type is the type of a pattern
(define-for-syntax (project-safe? t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [(_ (~Bind τ2:type))
     (and (finite? t1) (<: t1 #'τ2))]
    [(_ ~Discard)
     #t]
    [((~U τ1:type ...) _)
     (stx-andmap (lambda (t) (project-safe? t t2)) #'(τ1 ...))]
    [(_ (~U τ2:type ...))
     (stx-andmap (lambda (t) (project-safe? t1 t)) #'(τ2 ...))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (overlap? t1 t2)
     (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
    [((~Observe τ1:type) (~Observe τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [_ #t]))

;; AssertionType PatternType -> Bool
;; Is it possible for things of these two types to match each other?
;; Flattish-Type = Flat-Types + ★, Bind, Discard (assertion and pattern types)
(define-for-syntax (overlap? t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [(~★ _) #t]
    [(_ (~Bind _)) #t]
    [(_ ~Discard) #t]
    [((~U τ1:type ...) _)
     (stx-ormap (lambda (t) (overlap? t t2)) #'(τ1 ...))]
    [(_ (~U τ2:type ...))
     (stx-ormap (lambda (t) (overlap? t1 t)) #'(τ2 ...))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     (and (stx-length=? #'(τ1 ...) #'(τ2 ...))
          (stx-andmap overlap? #'(τ1 ...) #'(τ2 ...)))]
    [((~Observe τ1:type) (~Observe τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (overlap? #'τ1 #'τ2)]
    [_ (<: t1 t2)]))
    

;; Flattish-Type -> Bool
(define-for-syntax (finite? t)
  (syntax-parse t
    [~★ #f]
    [(~U τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~Tuple τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~Observe τ:type)
     (finite? #'τ)]
    [(~Inbound τ:type)
     (finite? #'τ)]
    [(~Outbound τ:type)
     (finite? #'τ)]
    [_ #t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements

(define-typed-syntax (set! x:id e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  [⊢ x ≫ x- ⇒ (~Field τ-x)]
  ;; TODO: (~ τ τ-x)
  -------------------------
  [⊢ (x- e-) (⇒ : (U)) (⇒ :2 (U)) (⇒ :3 (U))])

(define-typed-syntax (stop facet-name:id cont:stmt) ≫
  [⊢ facet-name ≫ facet-name- ⇐ FacetName]
  [⊢ cont ≫ cont- (⇒ : τ-i) (⇒ :2 τ-o) (⇒ :3 τ-a)]
  --------------------------------------------------
  [⊢ (syndicate:stop facet-name- cont-) (⇒ : τ-i) (⇒ :2 τ-o) (⇒ :3 τ-a)])

(define-typed-syntax (facet name:id ep:endpoint ...) ≫
  [[name ≫ name- : FacetName] ⊢ ep ≫ ep- (⇒ : τ-i) (⇒ :2 τ-o) (⇒ :3 τ-a)] ...
  --------------------------------------------------------------
  ;; name NOT name- here because I get an error that way.
  ;; Since name is just an identifier I think it's OK?
  [⊢ (syndicate:react (let- ([name (syndicate:current-facet-id)]) ep- ...))
     (⇒ : (U τ-i ...)) (⇒ :2 (U τ-o ...)) (⇒ :3 (U τ-a ...))])

(define-typed-syntax (dataspace τ-c:type s:stmt ...) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ : τ-i:type) (⇒ :2 τ-o:type) (⇒ :3 τ-s:type)] ...
  ;; #:do [(printf "dataspace types: ~a\n" (stx-map type->str #'(τ-s.norm ...)))
  ;;      (printf "dataspace type: ~a\n" (type->str ((current-type-eval) #'(Actor τ-c.norm))))]
  #:fail-unless (stx-andmap (lambda (t) (<: ((current-type-eval) #`(Actor #,t))
                                            ((current-type-eval) #'(Actor τ-c.norm))))
                            #'(τ-s.norm ...))
                "Not all actors conform to communication type"
  #:fail-unless (stx-andmap (lambda (t) (<: t ((current-type-eval) #'(U))))
                            #'(τ-i.norm ...)) "dataspace init should only be a spawn"
  #:fail-unless (stx-andmap (lambda (t) (<: t ((current-type-eval) #'(U))))
                            #'(τ-o.norm ...)) "dataspace init should only be a spawn"
  ;; TODO: check that each is is <: (Actor τ-c)
  #:with τ-ds-i (strip-inbound #'τ-c.norm)
  #:with τ-ds-o (strip-outbound #'τ-c.norm)
  #:with τ-relay (relay-interests #'τ-c.norm)
  ----------------------------------------------
  [⊢ (syndicate:dataspace s- ...) (⇒ : τ-ds-i) (⇒ :2 (U τ-ds-o τ-relay)) (⇒ :3 (U))])

(define-for-syntax (strip-? t)
  (syntax-parse t
    ;; TODO: probably need to `normalize` the result
    [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
    [~★ #'★]
    [(~Observe τ) #'τ]
    [_ #'(U)]))

(define-for-syntax (strip-inbound t)
  (syntax-parse t
    ;; TODO: probably need to `normalize` the result
    [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
    [~★ #'★]
    [(~Inbound τ) #'τ]
    [_ #'(U)]))

(define-for-syntax (strip-outbound t)
  (syntax-parse t
    ;; TODO: probably need to `normalize` the result
    [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
    [~★ #'★]
    [(~Outbound τ) #'τ]
    [_ #'(U)]))

(define-for-syntax (relay-interests t)
  (syntax-parse t
    ;; TODO: probably need to `normalize` the result
    [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
    [~★ #'★]
    [(~Observe (~Inbound τ)) #'(Observe τ)]
    [_ #'(U)]))

(define-typed-syntax (spawn τ-c:type s:stmt) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ : τ-i:type) (⇒ :2 τ-o:type) (⇒ :3 τ-a:type)]
  ;; TODO: s shouldn't refer to facets or fields!
  ;; TODO: should these be .norm ?
  #:with test-val (syntax-parse #'τ-o
                    [(~U τ-t:type ...) #'(τ-t ...)]
                    [_ #'#f])
  ;; #:do [(displayln (type->str #'τ-o))
  ;;      (displayln (type->str #'τ-c))]
  #:fail-unless (<: #'τ-o.norm #'τ-c.norm)
                (format "Output ~a not valid in dataspace ~a" (type->str #'τ-o) (type->str #'τ-c))
  #:fail-unless (<: ((current-type-eval) #'(Actor τ-a.norm))
                    ((current-type-eval) #'(Actor τ-c.norm))) "Spawned actors not valid in dataspace"
  #:fail-unless (project-safe? (∩ (strip-? ((current-type-eval) #'τ-o.norm))
                                  ((current-type-eval) #'τ-c.norm))
                               ((current-type-eval) #'τ-i.norm)) "Not prepared to handle all inputs"
  ----------------------------------------------
  [⊢ (syndicate:spawn (syndicate:on-start s-)) (⇒ : (U)) (⇒ :2 (U)) (⇒ :3 τ-c)])

(define-typed-syntax (let-field x:id (~datum :) τ:type e:expr body:expr) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  [[x ≫ x- : (Field τ)] ⊢ body ≫ body- (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)]
  #:fail-unless (flat-type? #'τ.norm) "Keep your functions out of fields"
  ------------------------------------------------------------------------
  [⊢ (let () (syndicate:field [x- e-]) body-) (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)])

(define-typed-syntax (let-function f:id (~datum :) τ:type e:expr body:expr) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  [[x ≫ x- : τ] ⊢ body ≫ body- (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)]
  #:fail-unless (procedure-type? #'τ.norm) "let-function only binds procedures"
   ------------------------------------------------------------------------
  [⊢ (let- ([x- e-]) body-) (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)])

(define-for-syntax (procedure-type? τ)
  (syntax-parse τ
    [(~Case (~→ τ1:type τ2:type) ...) #t]
    [_ #f]))

(define-typed-syntax (begin s:stmt ...) ≫
  [⊢ s ≫ s- (⇒ : τ1) (⇒ : τ2) (⇒ : τ3)] ...
  ------------------------------------------
  [⊢ (begin- (void-) s- ...) (⇒ : (U τ1 ...)) (⇒ :2 (U τ2 ...)) (⇒ :3 (U τ3 ...))])

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~Case _ ...) #f]
    [(~→ _ _) #f]
    [(~Field _) #f]
    [_ #t]))

(define-typed-syntax (unsafe-do rkt:expr ...) ≫
  ------------------------
  [⊢ (let- () rkt ...) (⇒ : (U)) (⇒ :2 (U)) (⇒ :3 (U))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Endpoints

#;(define-syntax-class endpoint
    #:datum-literals (on start stop)
    (pattern (~or (on ed:event-desc s:stmt)
                  (assert e:exp))))

(define-typed-syntax on
  [(on (~literal start) s:stmt) ≫
   [⊢ s ≫ s- (⇒ : τi) (⇒ :2 τ-o) (⇒ :3 τ-a)]
   -----------------------------------
   [⊢ (syndicate:on-start s-) (⇒ : τi) (⇒ :2 τ-o) (⇒ :3 τ-a)]]
  [(on (~literal stop) s:stmt) ≫
   [⊢ s ≫ s- (⇒ : τi) (⇒ :2 τ-o) (⇒ :3 τ-a)]
   -----------------------------------
   [⊢ (syndicate:on-stop s-) (⇒ : τi) (⇒ :2 τ-o) (⇒ :3 τ-a)]]
  ;; eww
  [(on ((~and a/p (~or (~literal asserted) (~literal retracted))) p:pat) s:stmt) ≫
   [⊢ p ≫ _ ⇒ τp]
   #:with pat-sub (replace-bind-with-★ #'τp)
   #:with pat-constraint (replace-bind-with-type #'τp)
   [⊢ s ≫ s- (⇒ : τi) (⇒ :2 τ-o) (⇒ :3 τ-a)]
   -----------------------------------
   ;; TODO - hard codes asserted!
   [⊢ (syndicate:on (syndicate:asserted p.syndicate-pattern) s-)
      (⇒ : (U τi pat-constraint))
      (⇒ :2 (U (Observe pat-sub) τ-o))
      (⇒ :3 τ-a)]])

;; TODO
(define-for-syntax (replace-bind-with-★ t)
  t)

;; TODO
(define-for-syntax (replace-bind-with-type t)
  t)

(define-typed-syntax (assert e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  #:with τ-in (strip-? #'τ.norm)
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : τ-in) (⇒ :2 τ) (⇒ :3 (U))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define-typed-syntax (tuple e:exp ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  -----------------------
  [⊢ (list 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (ref x:id) ≫
  [⊢ x ≫ x- ⇒ (Field τ)]
  ------------------------
  [⊢ (x-) ⇒ τ])

(define-typed-syntax (λ [p:pat s:stmt] ...) ≫
  #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ] ... ⊢ s ≫ s- (⇒ : τ1) (⇒ :2 τ2) (⇒ :3 τ3)] ...
  ;; REALLY not sure how to handle p/p-/p.match-pattern,
  ;; particularly w.r.t. typed terms that appear in p.match-pattern
  [⊢ p ≫ p- ⇒ τ-p] ...
  --------------------------------------------------------------
  ;; TODO: add a catch-all error clause
  [⊢ (match-lambda- [p.match-pattern s-] ...) ⇒ (Case [→ τ-p (Facet τ1 τ2 τ3)] ...)])

;; it would be nice to abstract over these three
(define-typed-syntax (observe e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (syndicate:observe e-) ⇒ (Observe τ)])

(define-typed-syntax (inbound e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (syndicate:inbound e-) ⇒ (Inbound τ)])

(define-typed-syntax (outbound e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (syndicate:outbound e-) ⇒ (Outbound τ)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns

(define-typed-syntax (bind x:id τ:type) ≫
  --------------------
  ;; TODO: at some point put $ back in
  [⊢ (void-) ⇒ (Bind τ)])

(define-typed-syntax discard
  [_ ≫
   --------------------
   ;; TODO: change void to _
   [⊢ (void-) ⇒ Discard]])

;; pat -> ([Id Type] ...)
(define-for-syntax (pat-bindings stx)
  (syntax-parse stx
    #:datum-literals (bind tuple)
    [(bind x:id τ:type)
     #'([x τ])]
    [(tuple p:pat ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [_:pat
     #'()]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives

;; hmmm
(define-primop + (→ Int Int Int))
(define-primop - (→ Int Int Int))

(define-typed-syntax (displayln e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (displayln- e-) ⇒ (U)])
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
  ----------------
  [⊢ (#%datum- . n) ⇒ Int]]
  [(_ . b:boolean) ≫
  ----------------
  [⊢ (#%datum- . b) ⇒ Bool]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) ⇒ String]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

#;(define-syntax (begin/void-default stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx (void))]
    [(_ expr0 expr ...)
     (syntax/loc stx (begin- expr0 expr ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

;; WANTED UNIT TESTS
;; (check-true (<: #'(U String) #'String))
;; (check-true (<: #'(U (U)) #'String))
;; (check-true (<: #'(Actor (U (U))) #'(Actor String))
;; (check-true (<: #'(Actor (U (U))) #'(Actor (U (Observe ★) String)))
;; (check-true (<: ((current-type-eval) #'(U (U) (U))) ((current-type-eval) #'(U))))
;; (check-false (<: ((current-type-eval) #'(Actor (U (Observe ★) String Int)))
;;                  ((current-type-eval) #'(Actor (U (Observe ★) String)))))
;; (check-true (<: (Actor (U (Observe ★) String)) (Actor (U (Observe ★) String)))

(module+ test
  (check-type 1 : Int)

  (check-type (let-field x : Int 5 (begin)) : (U))
  (check-type (let-field x : Int 5 (begin)) :2 (U))
  (check-type (let-field x : Int 5 (begin)) :3 (U))

  (check-type (tuple 1 2 3) : (Tuple Int Int Int))

  (typecheck-fail (let-field x : Int 5
                             (let-field y : (Field Int) x (begin)))
                  #:with-msg "Keep your functions out of fields")

  (check-type (tuple discard 1 (bind x Int)) : (Tuple Discard Int (Bind Int)))

  #;(check-type (λ [(bind x Int) (begin)]) : (Case [→ (Bind Int) (Facet (U) (U) (U))]))
  #;(check-true (void? ((λ [(bind x Int) (begin)]) 1))))

(define bank-account
  #'(facet account
           (let-field [balance : Int 0]
                      ;; this bit isn't valid because `assert` is not a statement
                      (begin (assert (tuple "balance" (ref balance)))
                             (on (asserted (tuple "deposit" (bind amount Int)))
                                 (set! balance (+ (ref balance) amount)))))))

(define client
  #'(spawn
     (let-field [deposits : Int 3]
                (facet client
                       (on (asserted (tuple "balance" (bind amount Int)))
                           ;; this bit isn't valid because `assert` is not a statement
                           (begin (assert (tuple "deposit" 100))
                                  (set! deposits (- (ref deposits) 1))
                                  ;; also `if` isn't in the grammar
                                  (if (= (ref deposits) 0)
                                      (stop client (begin))
                                      (begin))))))))

(define-syntax (test-syntax-class stx)
  (syntax-parse stx
    [(_ e class:id)
     #`(let ()
         (define-syntax (test-result stx)
           (syntax-parse e
             [(~var _ class) #'#t]
             [_ #'#f]))
         (test-result))]))

#;(begin-for-syntax
    (displayln (syntax-parse ((current-type-eval) #'(U String))
                 [(~U τ ...)
                  #'(τ ...)]
                 [_ 'boo])))