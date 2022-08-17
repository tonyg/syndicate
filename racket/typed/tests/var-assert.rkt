#lang typed/syndicate

(require rackunit/turnstile)

(check-type
 (role-strings
  (start-facet X
    (field [state Bool #t])
    (assert (tuple (! state)))))
  : (List String)
  -> (list "(Role (X) (VarAssert state (--> (U False True) (Tuple (U False True))) (--> False (Tuple False)) (--> True (Tuple True))))"))

(define (spawn-B)
  (spawn
   (start-facet B
                (field [Bst Bool #t])
                (assert (tuple (! Bst))))))

(lambda ()
  (lift+define-role rA
  (start-facet A (on start (spawn-B))
    (field [Ast Bool #t])
    (assert (tuple (! Ast))))))
