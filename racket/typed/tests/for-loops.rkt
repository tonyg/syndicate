#lang typed/syndicate/roles

(require rackunit/turnstile)


(check-type (for/fold ([x 0])
                      ([y (list 1 2 3)])
              x)
            : Int
            ⇒ 0)

(check-type (for/fold ([x 0])
                      ([y (list 1 2 3)])
              y)
            : Int
            ⇒ 3)

(define-type-alias Inventory (List (Tuple String Int)))

(define inventory0 (list (tuple "The Wind in the Willows" 5)
                           (tuple "Catch 22" 2)
                           (tuple "Candide" 33)))
(check-type (for/fold ([stock 0])
                      ([item inventory0])
              (select 1 item))
            : Int
            ⇒ 33)

(check-type (for/fold ([stock 0])
                      ([item inventory0])
              (if (equal? "Catch 22" (select 0 item))
                  (select 1 item)
                  stock))
            : Int
            ⇒ 2)

(define (lookup [title : String]
                [inv : Inventory] -> Int)
  (for/fold ([stock 0])
            ([item inv])
    (if (equal? title (select 0 item))
        (select 1 item)
        stock)))

(check-type lookup : (→fn String Inventory Int))

(define (zip [xs : (List Int)]
             [ys : (List Int)])
  ((inst reverse (Tuple Int Int))
   (for/fold ([acc : (List (Tuple Int Int))
                   (list)])
             ([x xs]
              [y ys])
     (cons (tuple x y) acc))))

(check-type (zip (list 1 2 3) (list 4 5 6))
            : (List (Tuple Int Int))
            ⇒ (list (tuple 1 4) (tuple 2 5) (tuple 3 6)))

;; binding in #:when isn't handled
#;(define (zip-even [xs : (List Int)]
                  [ys : (List Int)])
  ((inst reverse (Tuple Int Int))
   (for/fold ([acc : (List (Tuple Int Int))
                   (list)])
             ([x xs]
              #:when (even? x)
              [y ys]
              #:when (even? y))
     (cons (tuple x y) acc))))
