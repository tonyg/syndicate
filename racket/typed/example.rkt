#lang typed/syndicate

(require turnstile/rackunit-typechecking)

#;(spawn
 (assert (list "hello")))

#;(spawn
 (on (asserted (list "hello"))
     (printf "hello\n")))

(check-type 1 : Int)

(check-type (let-field x : Int 5 nil) : ⊥)
(check-type (let-field x : Int 5 nil) :2 ⊥)
(check-type (let-field x : Int 5 nil) :3 ⊥)

(check-type (tuple 1 2 3) : (Tuple Int Int Int))

(typecheck-fail (let-field x : Int 5
                           (let-field y : (Field Int) x nil))
                #:with-msg "Keep your functions out of fields")

(check-type (tuple discard 1 (bind x Int)) : (Tuple Discard Int (Bind Int)))

(check-type (λ [(bind x Int) nil]) : (Case [→ (Bind Int) (Facet ⊥ ⊥ ⊥)]))