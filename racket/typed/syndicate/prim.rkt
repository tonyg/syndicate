#lang turnstile

(provide (all-defined-out)
         True False Bool
         (for-syntax (all-defined-out)))

(require "core-types.rkt")
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - (only-in racket/format ~a ~v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-base-types Zero NonZero String ByteString Symbol)

(define-type-alias Int (U Zero NonZero))

;; hmmm
(define-primop + (→fn Int Int Int))
(define-primop - (→fn Int Int Int))
(define-primop * (→fn Int Int Int))
(define-primop not (→fn Bool Bool))
(define-primop < (→fn Int Int Bool))
(define-primop > (→fn Int Int Bool))
(define-primop <= (→fn Int Int Bool))
(define-primop >= (→fn Int Int Bool))
(define-primop = (→fn Int Int Bool))
(define-primop even? (→fn Int Bool))
(define-primop odd? (→fn Int Bool))
(define-primop add1 (→fn Int Int))
(define-primop sub1 (→fn Int Int))
(define-primop max (→fn Int Int Int))
(define-primop min (→fn Int Int Int))
(define-primop zero? (→fn Int Bool))
(define-primop positive? (→fn Int Bool))
(define-primop negative? (→fn Int Bool))
(define-primop current-inexact-milliseconds (→fn Int))
(define-primop string=? (→fn String String Bool))

(define-primop bytes->string/utf-8 (→fn ByteString String))
(define-primop string->bytes/utf-8 (→fn String ByteString))
(define-primop gensym (→fn Symbol Symbol))
(define-primop symbol->string (→fn Symbol String))
(define-primop string->symbol (→fn String Symbol))

(define-typed-syntax (/ e1 e2) ≫
  [⊢ e1 ≫ e1- (⇐ : Int)]
  [⊢ e2 ≫ e2- (⇐ : Int)]
  #:fail-unless (pure? #'e1-) "expression not allowed to have effects"
  #:fail-unless (pure? #'e2-) "expression not allowed to have effects"
  ------------------------
  [⊢ (#%app- exact-truncate- (#%app- /- e1- e2-)) (⇒ : Int)])

;; I think defining `and` and `or` as primops doesn't work because they're syntax
(define-typed-syntax (and e ...) ≫
  [⊢ e ≫ e- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions not allowed to have effects"
  ------------------------
  [⊢ (and- e- ...) (⇒ : Bool)])

(define-typed-syntax (or e ...) ≫
  [⊢ e ≫ e- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions not allowed to have effects"
  ------------------------
  [⊢ (or- e- ...) (⇒ : Bool)])

(define-typed-syntax (equal? e1:expr e2:expr) ≫
  [⊢ e1 ≫ e1- (⇒ : τ1)]
  [⊢ e2 ≫ e2- (⇒ : τ2)]
  #:fail-unless (flat-type? #'τ1)
    (format "equality only available on flat data; got ~a" (type->str #'τ1))
  #:fail-unless (flat-type? #'τ2)
    (format "equality only available on flat data; got ~a" (type->str #'τ2))
  #:with int (∩ #'τ1 #'τ2)
  #:fail-unless (not (bot? #'int))
    (format "empty overlap between types ~a and ~a" (type->str #'τ1) (type->str #'τ2))
  #:fail-unless (pure? #'e1-) "expression not allowed to have effects"
  #:fail-unless (pure? #'e2-) "expression not allowed to have effects"
  ---------------------------------------------------------------------------
  [⊢ (#%app- equal?- e1- e2-) (⇒ : Bool)])

(define-typed-syntax (displayln e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  ---------------
  [⊢ (#%app- displayln- e-) (⇒ : ★/t)])

(define-typed-syntax (printf e ...+) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expression not allowed to have effects"
  ---------------
  [⊢ (#%app- printf- e- ...) (⇒ : ★/t)])

(define-typed-syntax (~a e ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap flat-type? #'(τ ...))
                "expressions must be string-able"
  --------------------------------------------------
  [⊢ (#%app- ~a- e- ...) (⇒ : String)])

(define-typed-syntax (~v e ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap flat-type? #'(τ ...))
  "expressions must be string-able"
  --------------------------------------------------
  [⊢ (#%app- ~v- e- ...) (⇒ : String)])

(define-typed-syntax (format s e ...) ≫
  [⊢ s ≫ s- (⇐ : String)]
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap flat-type? #'(τ ...))
  "expressions must be string-able"
  --------------------------------------------------
  [⊢ (#%app- format- s- e- ...) (⇒ : String)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
   #:with T (if (zero? (syntax-e #'n)) #'Zero #'NonZero)
  ----------------
  [⊢ (#%datum- . n) (⇒ : T)]]
  [(_ . b:boolean)
   ≫
   #:with T (if (syntax-e #'b) #'True #'False)
  ----------------
  [⊢ (#%datum- . b) (⇒ : T)]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) (⇒ : String)]])

(define-typed-syntax (typed-quote x:id) ≫
  -------------------------------
  [⊢ (quote- x) (⇒ : Symbol)])
