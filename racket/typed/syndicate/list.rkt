#lang turnstile

(provide List
         (for-syntax ~List)
         list
         (typed-out [[empty- : (List ⊥)] empty]
                    [[empty?- : (∀ (X) (→fn (List X) Bool))] empty?]
                    [[cons- : (∀ (X Y) (→fn X (List Y) (List (U X Y))))] cons]
                    [[cons?- : (∀ (X) (→fn X (List X) Bool))] cons?]
                    [[first- : (∀ (X) (→fn (List X) X))] first]
                    [[second- : (∀ (X) (→fn (List X) X))] second]
                    [[rest- : (∀ (X) (→fn (List X) (List X)))] rest]
                    [[member?- (∀ (X) (→fn X (List X) Bool))] member?]
                    [[reverse- (∀ (X) (→fn (List X) (List X)))] reverse]
                    [[partition- (∀ (X) (→fn (List X) (→fn X Bool) (List X)))] partition]
                    [[map- (∀ (X Y) (→fn (→fn X Y) (List X) (List Y)))] map]
                    [[argmax- : (∀ (X) (→fn (→fn X Int) (List X) X))] argmax]
                    [[argmin- : (∀ (X) (→fn (→fn X Int) (List X) X))] argmin]
                    [[remove- : (∀ (X) (→fn X (List X) (List X)))] remove]
                    [[length- : (∀ (X) (→fn (List X) Int))] length]))

(require "core-types.rkt")
(require (only-in "prim.rkt" Bool Int))
(require (postfix-in - racket/list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-container-type List #:arity = 1)

(define-typed-syntax (list e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
  -------------------
  [⊢ (#%app- list- e- ...) ⇒ (List (U τ ...))])

(define- (member?- v l)
  (and- (#%app- member- v l) #t))
