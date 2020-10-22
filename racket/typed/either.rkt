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
                                                       (Right Z)))]
                                     -> (Tuple (List Y) (List Z))))
  (for/fold ([lefts (List Y) (list)]
             [rights (List Z) (list)])
            ([x xs])
    (define y-or-z (pred x))
    (match y-or-z
      [(left (bind y Y))
       (tuple (cons y lefts)
              rights)]
      [(right (bind z Z))
       (tuple lefts
              (cons z rights))])))
