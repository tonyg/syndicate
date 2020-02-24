#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require "core-types.rkt")
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - (only-in racket/format ~a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-base-types Int Bool String ByteString Symbol)

;; hmmm
(define-primop + (→ Int Int (Computation (Value Int) (Endpoints) (Roles) (Spawns))))
(define-primop - (→ Int Int (Computation (Value Int) (Endpoints) (Roles) (Spawns))))
(define-primop * (→ Int Int (Computation (Value Int) (Endpoints) (Roles) (Spawns))))
(define-primop not (→ Bool (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop < (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop > (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop <= (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop >= (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop = (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop even? (→fn Int Bool))
(define-primop odd? (→fn Int Bool))
(define-primop add1 (→fn Int Int))
(define-primop sub1 (→fn Int Int))
(define-primop max (→fn Int Int Int))
(define-primop min (→fn Int Int Int))
(define-primop zero? (→fn Int Bool))
(define-primop positive? (→fn Int Bool))
(define-primop negative? (→fn Int Bool))

(define-primop bytes->string/utf-8 (→ ByteString (Computation (Value String) (Endpoints) (Roles) (Spawns))))
(define-primop string->bytes/utf-8 (→ String (Computation (Value ByteString) (Endpoints) (Roles) (Spawns))))
(define-primop gensym (→ Symbol (Computation (Value Symbol) (Endpoints) (Roles) (Spawns))))
(define-primop symbol->string (→ Symbol (Computation (Value String) (Endpoints) (Roles) (Spawns))))
(define-primop string->symbol (→ String (Computation (Value Symbol) (Endpoints) (Roles) (Spawns))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
  ----------------
  [⊢ (#%datum- . n) (⇒ : Int)]]
  [(_ . b:boolean) ≫
  ----------------
  [⊢ (#%datum- . b) (⇒ : Bool)]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) (⇒ : String)]])

(define-typed-syntax (typed-quote x:id) ≫
  -------------------------------
  [⊢ (quote- x) (⇒ : Symbol)])
