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
         else
         match
         tuple
         unit
         select
         error
         define-tuple
         match-define
         mk-tuple
         tuple-select
         (for-syntax (all-defined-out)))

(require "core-types.rkt")
(require (only-in "prim.rkt" Bool String #%datum))
(require (postfix-in - racket/match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  [⊢ (#%app- error- 'bind "escaped") (⇒ : (Bind τ))])

(define-typed-syntax (discard τ:type) ≫
  ----------------------------------------
  [⊢ (#%app- error- 'discard "escaped") (⇒ : (Discard τ.norm))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core-ish forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copied from stlc
(define-typed-syntax (ann e (~optional (~datum :)) τ:type) ≫
  [⊢ e ≫ e- (⇐ : τ.norm) (⇒ ν (~effs eff ...))]
  ------------------------------------------------
  [⊢ e- (⇒ : τ.norm) (⇒ ν (eff ...))])

;; copied from ext-stlc
(define-typed-syntax if
  [(_ e_tst e1 e2) ⇐ τ-expected ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇐ : τ-expected) (⇒ ν (~effs eff1 ...))]
   [⊢ e2 ≫ e2- (⇐ : τ-expected) (⇒ ν (~effs eff2 ...))]
   --------
   [⊢ (if- e_tst- e1- e2-)
      (⇒ : τ-expected)
      (⇒ ν #,(make-Branch #'((eff1 ...) (eff2 ...))))]]
  [(_ e_tst e1 e2) ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇒ : τ1) (⇒ ν (~effs eff1 ...))]
   [⊢ e2 ≫ e2- (⇒ : τ2) (⇒ ν (~effs eff2 ...))]
   #:with τ (mk-U- #'(τ1 τ2))
   --------
   [⊢ (if- e_tst- e1- e2-) (⇒ : τ)
      (⇒ ν #,(make-Branch #'((eff1 ...) (eff2 ...))))]])

(define-typed-syntax (when e s ...+) ≫
  ------------------------------------
  [≻ (if e (let () s ...) #f)])

(define-typed-syntax (unless e s ...+) ≫
  ------------------------------------
  [≻ (if e #f (let () s ...))])


;; copied from ext-stlc
(define-typed-syntax let
  [(_ ([x e] ...) e_body ...) ⇐ τ_expected ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (block e_body ...) ≫ e_body- (⇐ : τ_expected)
                   (⇒ ν (~effs eff ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-) (⇒ : τ_expected)
      (⇒ ν (eff ...))]]
  [(_ ([x e] ...) e_body ...) ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (block e_body ...) ≫ e_body- (⇒ : τ_body)
                   (⇒ ν (~effs eff ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-) (⇒ : τ_body)
      (⇒ ν (eff ...))]])

;; copied from ext-stlc
(define-typed-syntax let*
  [(_ () e_body ...) ≫
   --------
   [≻ (block e_body ...)]]
  [(_ ([x e] [x_rst e_rst] ...) e_body ...) ≫
   --------
   [≻ (let ([x e]) (let* ([x_rst e_rst] ...) e_body ...))]])

(define-typed-syntax (cond [pred:expr s ...+] ...+) ≫
  [⊢ pred ≫ pred- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(pred- ...)) "predicates must be pure"
  [⊢ (block s ...) ≫ s- (⇒ : τ-s)
                   (⇒ ν (~effs eff ...))] ...
  ------------------------------------------------
  [⊢ (cond- [pred- s-] ...) (⇒ : (U τ-s ...))
     (⇒ ν #,(make-Branch #'((eff ...) ...)))])

(define else #t)

(define-typed-syntax (match e [p s ...+] ...+) ≫
  [⊢ e ≫ e- (⇒ : τ-e)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  #:with (p/e ...) (for/list ([pat (in-syntax #'(p ...))])
                     (elaborate-pattern/with-type pat #'τ-e))
  #:with (([x τ:type] ...) ...) (stx-map pat-bindings #'(p/e ...))
  [[x ≫ x- : τ.norm] ... ⊢ (block s ...) ≫ s- (⇒ : τ-s)
                (⇒ ν (~effs eff ...))] ...
  ;; REALLY not sure how to handle p/p-/p.match-pattern,
  ;; particularly w.r.t. typed terms that appear in p.match-pattern
  [⊢ p/e ≫ p-- ⇒ τ-p] ...
  #:fail-unless (project-safe? #'τ-e (mk-U*- #'(τ-p ...))) "possibly unsafe pattern match"
  #:fail-unless (stx-andmap pure? #'(p-- ...)) "patterns must be pure"
  #:with (p- ...) (stx-map (lambda (p x-s xs) (substs x-s xs (compile-match-pattern p)))
                           #'(p/e ...)
                           #'((x- ...) ...)
                           #'((x ...) ...))
  --------------------------------------------------------------
  [⊢ (match- e- [p- s-] ...
                [_ (#%app- error- "incomplete pattern match")])
     (⇒ : (U τ-s ...))
     (⇒ ν #,(make-Branch #'((eff ...) ...)))])


;; (Listof Value) -> Value
(define- (mk-tuple es)
  (#%app- cons- 'tuple es))

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν (~effs F ...))] ...
  -----------------------
  [⊢ (#%app- mk-tuple (#%app- list- e- ...))
     (⇒ : (Tuple τ ...))
     (⇒ ν (F ... ...))])

(define unit : Unit (tuple))

(define-typed-syntax (select n:nat e:expr) ≫
  [⊢ e ≫ e- (⇒ : (~Tuple τ ...)) (⇒ ν (~effs F ...))]
  #:do [(define i (syntax->datum #'n))]
  #:fail-unless (< i (stx-length #'(τ ...))) "index out of range"
  #:with τr (list-ref (stx->list #'(τ ...)) i)
  --------------------------------------------------------------
  [⊢ (#%app- tuple-select n e-) (⇒ : τr) (⇒ ν (F ...))])

(define- (tuple-select n t)
  (#%app- list-ref- t (#%app- add1- n)))

(define-typed-syntax (error msg args ...) ≫
  [⊢ msg ≫ msg- (⇐ : String)]
  [⊢ args ≫ args- (⇒ : τ)] ...
  #:fail-unless (all-pure? #'(msg- args- ...)) "expressions must be pure"
  ----------------------------------------
  [⊢ (#%app- error- msg- args- ...) (⇒ : ⊥)])

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
    [(~constructor-exp cons p ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [_
     #'()]))

(begin-for-syntax
  ;; Any -> Bool
  (define (dollar-variable? x)
    (and (identifier? x)
         (char=? (string-ref (symbol->string (syntax-e x)) 0) #\$)))

  ;; dollar-id -> Identifier
  (define (un-dollar x)
    (datum->syntax x (string->symbol (substring (symbol->string (syntax-e x)) 1))))

  (define-syntax-class dollar-id
    #:attributes (id)
    (pattern x:id
             #:when (dollar-variable? #'x)
             #:attr id (un-dollar #'x)))

  ;; match things of the form "$X:Y" where X and Y are things without
  ;; spaces (i.e. likely but not definitely legal identifiers)
  (define DOLLAR-ANN-RX #px"^\\$(\\S*):(\\S*)$")

  ;; Any -> RegexpMatchResults
  (define (dollar-ann-variable? x)
    (and (identifier? x)
         (regexp-match DOLLAR-ANN-RX (symbol->string (syntax-e x)))))

  (define-syntax-class dollar-ann-id
    #:attributes (id ty)
    (pattern x:id
             #:do [(define match? (dollar-ann-variable? #'x))]
             #:when match?
             #:attr id (datum->syntax #'x (string->symbol (second match?)))
             #:attr ty (datum->syntax #'x (string->symbol (third match?)))))

  ;; match things of the form "_:Y" where Y is a type without
  ;; spaces (i.e. likely but not definitely legal identifiers)
  (define DISCARD-ANN-RX #px"^_:(\\S*)$")

  ;; Any -> RegexpMatchResults
  (define (discard-ann-variable? x)
    (and (identifier? x)
         (regexp-match DISCARD-ANN-RX (symbol->string (syntax-e x)))))

  (define-syntax-class discard-ann-id
    #:attributes (ty)
    (pattern x:id
             #:do [(define match? (discard-ann-variable? #'x))]
             #:when match?
             #:attr ty (datum->syntax #'x (string->symbol (second match?)))))

  ;; expand uses of $ short-hand
  ;; doesn't handle uses of $id or ($) w/o a type
  (define (elaborate-pattern pat)
    (syntax-parse pat
      #:datum-literals (tuple _ $ ★)
      [x:discard-ann-id
       (syntax/loc pat (discard x.ty))]
      [(~or* _ ★)
       (syntax/loc pat (discard ★/t))]
      [x:dollar-ann-id
       (syntax/loc pat (bind x.id x.ty))]
      [($ x:id ty)
       (syntax/loc pat (bind x ty))]
      [(tuple p ...)
       (quasisyntax/loc pat
         (tuple #,@(stx-map elaborate-pattern #'(p ...))))]
      [(~constructor-exp ctor p ...)
       (define field-tys (ctor-field-tys #'ctor))
       (define sub-pats
         (for/list ([field-pat (in-syntax #'(p ...))]
                    [field-ty? (in-list field-tys)])
           (if field-ty?
               (elaborate-pattern/with-type field-pat field-ty?)
               (elaborate-pattern field-pat))))
       (quasisyntax/loc pat
         (ctor #,@sub-pats))]
      [e:expr
       pat]))

  (define (elaborate-pattern/with-type pat ty)
    (syntax-parse pat
      #:datum-literals (tuple $ ★)
      [(~datum _)
       (when (bot? ty)
         (raise-syntax-error #f "unable to instantiate pattern with matching part of type" pat))
       (quasisyntax/loc pat (discard #,ty))]
      [x:discard-ann-id
       (syntax/loc pat (discard x.ty))]
      [★
       (syntax/loc pat (discard ★/t))]
      [x:dollar-ann-id
       (syntax/loc pat (bind x.id x.ty))]
      [x:dollar-id
       (when (bot? ty)
         (raise-syntax-error #f "unable to instantiate pattern with matching part of type" pat))
       (quasisyntax/loc pat (bind x.id #,ty))]
      [($ x:id ty)
       (syntax/loc pat (bind x ty))]
      [($ x:id)
       (when (bot? ty)
         (raise-syntax-error #f "unable to instantiate pattern with matching part of type" pat))
       (quasisyntax/loc pat (bind x #,ty))]
      [(tuple p ...)
       (define (matching? t)
         (syntax-parse t
           [(~Tuple tt ...)
            #:when (stx-length=? #'(p ...) #'(tt ...))
            #t]
           [_ #f]))
       (define (proj t i)
         (syntax-parse t
           [(~Tuple tt ...)
            (if (= i -1)
                t
                (stx-list-ref #'(tt ...) i))]
           [(~U* (~or (~and tt (~fail #:unless (or (U*? #'tt) (matching? #'tt))))
                      _) ...)
            (mk-U- (stx-map (lambda (x) (proj x i)) #'(tt ...)))]
           [_
            (mk-U*- '())]))
       (define selected (proj ty -1))
       (define sub-pats
         (for/list ([pat (in-syntax #'(p ...))]
                    [i (in-naturals)])
           (elaborate-pattern/with-type pat (proj selected i))))
       (quasisyntax/loc pat
         (tuple #,@sub-pats))]
      [(~constructor-exp ctor p ...)
       (define tag (ctor-type-tag #'ctor))
       (define (matching? t)
         (syntax-parse t
           [(~constructor-type tag2 tt ...)
            #:when (equal? tag (syntax-e #'tag2))
            #:when (stx-length=? #'(p ...) #'(tt ...))
            #t]
           [_ #f]))
       (define (proj t i)
         (syntax-parse t
           [(~constructor-type _ tt ...)
            #:when (matching? t)
            (if (= i -1)
                t
                (stx-list-ref #'(tt ...) i))]
           [(~U* (~or (~and tt (~fail #:unless (or (U*? #'tt) (matching? #'tt))))
                      _) ...)
            (mk-U- (stx-map (lambda (x) (proj x i)) #'(tt ...)))]
           [_
            (mk-U*- '())]))
       (define selected (proj ty -1))
       (define sub-pats
         (for/list ([pat (in-syntax #'(p ...))]
                    [i (in-naturals)])
           (elaborate-pattern/with-type pat (proj selected i))))
       (quasisyntax/loc pat
         (ctor #,@sub-pats))]
      [e:expr
       #'e])))

;; TODO - figure out why this needs different list identifiers
(define-for-syntax (compile-pattern pat list-binding bind-id-transformer exp-transformer)
    (define (l-e stx) (local-expand stx 'expression '()))
    (let loop ([pat pat])
      (syntax-parse pat
        #:datum-literals (tuple discard bind ★)
        [(tuple p ...)
         #`(#,list-binding 'tuple #,@(stx-map loop #'(p ...)))]
        [(bind x:id τ:type)
         (bind-id-transformer #'x)]
        [(~or* (discard _) ★)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (define-tuple (x:id ...) e:expr) ≫
  [⊢ e ≫ e- (⇒ (~Tuple τ ...))]
  #:fail-unless (stx-length=? #'(x ...) #'(τ ...))
                "mismatched size"
  #:with (sel ...) (for/list ([y (in-syntax #'(x ...))]
                              [t (in-syntax #'(τ ...))]
                              [i (in-naturals)])
                     (quasisyntax/loc this-syntax
                       (select #,i it)))
  ----------------------------------------
  [≻ (begin
       (define it e-)
       (define x : τ sel) ...)])

(define-typed-syntax (match-define pat:expr e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ-e)]
  #:with pat+ (elaborate-pattern/with-type #'pat #'τ-e)
  #:with ([x τ] ...) (pat-bindings #'pat+)
  ----------------------------------------
  [≻ (define-tuple (x ...)
       (match e-
         [pat+
          (tuple x ...)]))])

;; extremely limited match-define for `define-constructor`-d things

#;(define-typed-syntax (match-define (~constructor-exp ctor x:id ...) e:expr) ≫
  [⊢ e ≫ e- (⇒ (~constructor-type tag1 τ ...))]
  #:fail-unless (stx-length=? #'(x ...) #'(τ ...))
                "mismatched size"
  [⊢ (ctor (bind x τ) ...) ≫ pat- (⇒ (~constructor-type tag2 _ ...))]
  #:fail-unless (equal? #'tag1 #'tag2)
                (~format "type mismatch: ~a, ~a" #'tag1 #'tag2)
  ------------------------------------------------------------
  )
