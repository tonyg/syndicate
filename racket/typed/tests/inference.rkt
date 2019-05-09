#lang typed/syndicate/roles

(require rackunit/turnstile)

(define (∀ (X) (poly-cons [x : X]
                          [xs : (List X)]
                          -> (List X)))
  (cons x xs))

(define int-list : (List Int) (list 1 2 3))

(check-type (poly-cons 0 int-list)
            : (List Int)
            -> (list 0 1 2 3))

(define string-list : (List String) (list "group" "of" "helpful" "badgers"))

(check-type (poly-cons "a" string-list)
            : (List String)
            -> (list "a" "group" "of" "helpful" "badgers"))

(typecheck-fail (poly-cons "hello" int-list))

(define string-int-list : (List (U String Int))
  (list "hi" 42 "badgers"))

;; shouldn't mess about with unions
(typecheck-fail (poly-cons "go" string-int-list))

