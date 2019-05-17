#lang turnstile

(provide bind
         discard
         ann
         if
         when
         unless
         let
         let*
         cond
         match
         tuple
         select
         (for-syntax (all-defined-out)))

(require "core-types.rkt")
(require (only-in "prim.rkt" Bool #%datum))
(require (postfix-in - racket/match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  [⊢ (error- 'bind "escaped") (⇒ : (Bind τ))])

(define-typed-syntax discard
  [_ ≫
   --------------------
   [⊢ (error- 'discard "escaped") (⇒ : Discard)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core-ish forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copied from stlc
(define-typed-syntax (ann e (~optional (~datum :)) τ:type) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ------------------------------------------------
  [⊢ e- (⇒ : τ.norm) ])

;; copied from ext-stlc
(define-typed-syntax if
  [(_ e_tst e1 e2) ⇐ τ-expected ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇐ : τ-expected)
      (⇒ ν-ep (~effs eps1 ...)) (⇒ ν-f (~effs fs1 ...)) (⇒ ν-s (~effs ss1 ...))]
   [⊢ e2 ≫ e2- (⇐ : τ-expected)
      (⇒ ν-ep (~effs eps2 ...)) (⇒ ν-f (~effs fs2 ...)) (⇒ ν-s (~effs ss2 ...))]
   --------
   [⊢ (if- e_tst- e1- e2-)
      (⇒ : τ-expected)
      (⇒ ν-ep (eps1 ... eps2 ...))
      (⇒ ν-f #,(make-Branch #'((fs1 ...) (fs2 ...))))
      (⇒ ν-s (ss1 ... ss2 ...))]]
  [(_ e_tst e1 e2) ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇒ : τ1)
      (⇒ ν-ep (~effs eps1 ...)) (⇒ ν-f (~effs fs1 ...)) (⇒ ν-s (~effs ss1 ...))]
   [⊢ e2 ≫ e2- (⇒ : τ2)
      (⇒ ν-ep (~effs eps2 ...)) (⇒ ν-f (~effs fs2 ...)) (⇒ ν-s (~effs ss2 ...))]
   #:with τ (type-eval #'(U τ1 τ2))
   --------
   [⊢ (if- e_tst- e1- e2-) (⇒ : τ)
      (⇒ ν-ep (eps1 ... eps2 ...))
      (⇒ ν-f #,(make-Branch #'((fs1 ...) (fs2 ...))))
      (⇒ ν-s (ss1 ... ss2 ...))]])

(define-typed-syntax (when e s ...+) ≫
  ------------------------------------
  [≻ (if e (begin s ...) #f)])

(define-typed-syntax (unless e s ...+) ≫
  ------------------------------------
  [≻ (if e #f (begin s ...))])

;; copied from ext-stlc
(define-typed-syntax let
  [(_ ([x e] ...) e_body ...) ⇐ τ_expected ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- (⇐ : τ_expected)
                   (⇒ ν-ep (~effs eps ...))
                   (⇒ ν-f (~effs fs ...))
                   (⇒ ν-s (~effs ss ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-) (⇒ : τ_expected)
      (⇒ ν-ep (eps ...))
      (⇒ ν-f (fs ...))
      (⇒ ν-s (ss ...))]]
  [(_ ([x e] ...) e_body ...) ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- (⇒ : τ_body)
                   (⇒ ν-ep (~effs eps ...))
                   (⇒ ν-f (~effs fs ...))
                   (⇒ ν-s (~effs ss ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-) (⇒ : τ_body)
      (⇒ ν-ep (eps ...))
      (⇒ ν-f (fs ...))
      (⇒ ν-s (ss ...))]])

;; copied from ext-stlc
(define-typed-syntax let*
  [(_ () e_body ...) ≫
   --------
   [≻ (begin e_body ...)]]
  [(_ ([x e] [x_rst e_rst] ...) e_body ...) ≫
   --------
   [≻ (let ([x e]) (let* ([x_rst e_rst] ...) e_body ...))]])

(define-typed-syntax (cond [pred:expr s ...+] ...+) ≫
  [⊢ pred ≫ pred- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(pred- ...)) "predicates must be pure"
  [⊢ (begin s ...) ≫ s- (⇒ : τ-s)
                   (⇒ ν-ep (~effs eps ...))
                   (⇒ ν-f (~effs fs ...))
                   (⇒ ν-s (~effs ss ...))] ...
  ------------------------------------------------
  [⊢ (cond- [pred- s-] ...) (⇒ : (U τ-s ...))
     (⇒ ν-ep (eps ... ...))
     (⇒ ν-f #,(make-Branch #'((fs ...) ...)))
     (⇒ ν-s (ss ... ...))])

(define-typed-syntax (match e [p s ...+] ...+) ≫
  [⊢ e ≫ e- (⇒ : τ-e)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  #:with (([x τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ.norm] ... ⊢ (begin s ...) ≫ s- (⇒ : τ-s)
                (⇒ ν-ep (~effs eps ...))
                (⇒ ν-f (~effs fs ...))
                (⇒ ν-s (~effs ss ...))] ...
  ;; REALLY not sure how to handle p/p-/p.match-pattern,
  ;; particularly w.r.t. typed terms that appear in p.match-pattern
  [⊢ p ≫ p-- ⇒ τ-p] ...
  #:fail-unless (project-safe? #'τ-e (mk-U*- #'(τ-p ...))) "possibly unsafe pattern match"
  #:fail-unless (stx-andmap pure? #'(p-- ...)) "patterns must be pure"
  #:with (p- ...) (stx-map (lambda (p x-s xs) (substs x-s xs (compile-match-pattern p)))
                           #'(p ...)
                           #'((x- ...) ...)
                           #'((x ...) ...))
  --------------------------------------------------------------
  [⊢ (match- e- [p- s-] ...
                [_ (#%app- error "incomplete pattern match")])
     (⇒ : (U τ-s ...))
     (⇒ ν-ep (eps ... ...))
     (⇒ ν-f #,(make-Branch #'((fs ...) ...)))
     (⇒ ν-s (ss ... ...))])

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions not allowed to have effects"
  -----------------------
  [⊢ (#%app- list- 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (select n:nat e:expr) ≫
  [⊢ e ≫ e- (⇒ : (~Tuple τ ...))]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:do [(define i (syntax->datum #'n))]
  #:fail-unless (< i (stx-length #'(τ ...))) "index out of range"
  #:with τr (list-ref (stx->list #'(τ ...)) i)
  --------------------------------------------------------------
  [⊢ (#%app- tuple-select n e-) (⇒ : τr)])

(define- (tuple-select n t)
  (#%app- list-ref- t (#%app- add1- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pat -> ([Id Type] ...)
(define-for-syntax (pat-bindings stx)
  (syntax-parse stx
    #:datum-literals (bind tuple)
    [(bind x:id τ:type)
     #'([x τ])]
    [(tuple p ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [(k:kons1 p)
     (pat-bindings #'p)]
    [(~constructor-exp cons p ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [_
     #'()]))

;; TODO - figure out why this needs different list identifiers
(define-for-syntax (compile-pattern pat list-binding bind-id-transformer exp-transformer)
    (define (l-e stx) (local-expand stx 'expression '()))
    (let loop ([pat pat])
      (syntax-parse pat
        #:datum-literals (tuple discard bind)
        [(tuple p ...)
         #`(#,list-binding 'tuple #,@(stx-map loop #'(p ...)))]
        [(k:kons1 p)
         #`(#,(kons1->constructor #'k) #,(loop #'p))]
        [(bind x:id τ:type)
         (bind-id-transformer #'x)]
        [discard
         #'_]
        [(~constructor-exp ctor p ...)
         (define/with-syntax uctor (untyped-ctor #'ctor))
         #`(uctor #,@(stx-map loop #'(p ...)))]
        [_
         ;; local expanding "expression-y" syntax allows variable references to transform
         ;; according to the mappings set up by turnstile.
         (exp-transformer (l-e pat))])))

(define-for-syntax (compile-match-pattern pat)
  (compile-pattern pat
                   #'list
                   identity
                   (lambda (exp) #`(==- #,exp))))
