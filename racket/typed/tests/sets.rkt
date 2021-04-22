#lang typed/syndicate

(require rackunit/turnstile)

(check-type (set 1 2 3)
            : (Set Int)
            -> (set 2 3 1))
(check-type (set 1 "hello" 3)
            : (Set (U Int String))
            -> (set "hello" 3 1))
(check-type (set-count (set 1 "hello" 3))
            : Int
            -> 3)
(check-type (set-union (set 1 2 3) (set "hello" "world"))
            : (Set (U Int String))
            -> (set 1 2 3 "hello" "world"))
(check-type (set-intersect (set 1 2 3) (set "hello" "world"))
            : (Set ⊥)
            -> (set))
(check-type (set-intersect (set 1 "hello" 3) (set #t "world" #f "hello"))
            : (Set String)
            -> (set "hello"))
