#lang turnstile

(provide Left
         Right
         Either
         left
         right
         partition/either)

(require "core-types.rkt")
(require "core-expressions.rkt")
(require "for-loops.rkt")
(require "list.rkt")

(define-constructor* (left : Left v))
(define-constructor* (right : Right v))

(define-type-alias (Either A B)
  (U (Left A)
     (Right B)))

(define (∀ (X) (f [x : X] -> X))
  x)


(define (∀ (X Y Z) (partition/either [xs : (List X)]
                                     [pred : (→fn X (U (Left Y)
                                                       (Right Z)) #;(Either Y Z))]
                                     -> (Tuple (List Y) (List Z))))
  (for/fold ([acc (Tuple (List Y) (List Z)) (tuple (list) (list))])
            ([x xs])
    (define y-or-z (pred x))
    (match y-or-z
      [(left (bind y Y))
       (tuple (cons y (select 0 acc))
              (select 1 acc))]
      [(right (bind z Z))
       (tuple (select 0 acc)
              (cons z (select 1 acc)))])))
