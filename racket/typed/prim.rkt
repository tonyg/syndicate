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
(define-primop or (→ Bool Bool (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop not (→ Bool (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop < (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop > (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop <= (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop >= (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop = (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop even? (→fn Int Bool))
(define-primop odd? (→fn Int Bool))

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
  [⊢ (exact-truncate- (/- e1- e2-)) (⇒ : Int)])

;; for some reason defining `and` as a prim op doesn't work
(define-typed-syntax (and e ...) ≫
  [⊢ e ≫ e- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions not allowed to have effects"
  ------------------------
  [⊢ (and- e- ...) (⇒ : Bool)])

(define-typed-syntax (equal? e1:expr e2:expr) ≫
  [⊢ e1 ≫ e1- (⇒ : τ1)]
  #:fail-unless (flat-type? #'τ1)
  (format "equality only available on flat data; got ~a" (type->str #'τ1))
  [⊢ e2 ≫ e2- (⇐ : τ1)]
  #:fail-unless (pure? #'e1-) "expression not allowed to have effects"
  #:fail-unless (pure? #'e2-) "expression not allowed to have effects"
  ---------------------------------------------------------------------------
  [⊢ (equal?- e1- e2-) (⇒ : Bool)])

(define-typed-syntax (displayln e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  ---------------
  [⊢ (displayln- e-) (⇒ : ★/t)])

(define-typed-syntax (printf e ...+) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expression not allowed to have effects"
  ---------------
  [⊢ (printf- e- ...) (⇒ : ★/t)])

(define-typed-syntax (~a e ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap flat-type? #'(τ ...))
                "expressions must be string-able"
  --------------------------------------------------
  [⊢ (~a- e- ...) (⇒ : String)])

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
