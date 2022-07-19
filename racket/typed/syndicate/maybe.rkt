#lang turnstile

(provide Maybe
         None
         None*
         Some
         some
         none
         has?)

(require "core-types.rkt")
(require "prim.rkt")
(require "core-expressions.rkt")


(define-constructor* (none* : None*))
(define-constructor* (some : Some v))

(define-type-alias None (None*))

(define none : None
  (none*))

(define-type-alias (Maybe X)
  (U None
     (Some X)))

#;(define (∀ (X Y) (partition/maybe [xs : (List X)]
                                  [pred : (→fn X (Maybe Y))]
                                  -> (Tuple (List Y) (List X))))
    #f)

#;(require (only-in "core-expressions.rkt" match error discard)
         "prim.rkt")
#;(define (∀ (X) (unwrap! [x : (Maybe X)] -> (Maybe X)))
  (match x
    [(some discard)
     (error "some")]
    [none
     (error "none")]))

(define (∀ (X) (has? [v : (Maybe X)] [p : (→fn X Bool)] -> Bool))
  (match v
    [none
     #f]
    [(some $x)
     (p x)]))
