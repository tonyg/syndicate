#lang typed/syndicate

(define-constructor* (invitation))
(define-constructor* (conversation))
(define-constructor* (permitted))
(define-constructor* (question [a : Bool] [b : Bool] [c : Bool] [d : Bool] [e : Bool] [f : Bool] [g : Bool] [h : Bool] [i : Bool]))
(define-constructor* (answer))

(define (spawnA)
  (spawn
    (during (invitation)
      (during (conversation)
        (during (permitted)
          (assert (question #t #t #t #t #t #t #t #t #t))
          (stop-when (asserted (answer))))))))

(define (spawnB)
  (spawn
    (assert (tuple))))

(module+ test
  (define-type-alias IO-types
    (U Invitation
       Conversation
       Permitted
       Question
       Answer))
  (define-ltl spec
    (Always (Implies (And (And Invitation Question (Not Answer))
                          (Until (And Invitation Question (Not Answer))
                                 (Not Invitation)))
                     (Eventually (Not Question)))))
  (verify-actors/fail spec
    #:IO IO-types
    (spawnA)))
