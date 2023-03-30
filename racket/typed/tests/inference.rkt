#lang typed/syndicate

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

;; fails because unification is too strict, requiring equality as opposed to
;; upper&lower bounds
(check-type (poly-cons (ann "go" (U String Int)) string-int-list)
            : (List (U String Int)))
(typecheck-fail (poly-cons "go" string-int-list))

(typecheck-fail (poly-cons (lambda () 0) (ann (list) (List (→fn Int))))
                #:with-msg "type variables must be flat and finite")

;; Failure because inference doesn't handle variables under unions
(define (∀ (X) (unwrap! [x : (Maybe X)] -> X))
  (match x
    [(some (bind v X))
     v]
    [none
     (error "none")]))

(typecheck-fail (unwrap! (some 5))
                #:with-msg "can't infer types with unions")

(check-type ((inst unwrap! Int) (some 5))
            : Int
            -> 5)

(check-type (map add1 (list 0 1 2 3))
            : (List Int))
(check-type (map zero? (list 0 1 2 3))
            : (List Bool))

(define (∀ (X) (id [x : X])) x)
(typecheck-fail (map id (list))
                #:with-msg "couldn't unify")
(check-type (map (inst id ⊥) (list))
            : (List ⊥))

(define-constructor* (layout-item [size : Int]))
(check-type (map (inst layout-item-size Int) (list (layout-item 1) (layout-item 2)))
            : (List Int))

