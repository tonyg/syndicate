#lang typed/syndicate

(define-type-alias Room Symbol)
(define-constructor* (avatar-location [r : Room]))
(define-constructor* (move-cmd [room : Room]))

(define-constructor* (turn-on))
(define-constructor* (turn-off))

(define (spawn-avatar [room0 : Room])
  (spawn
    (start-facet avatar-impl
      (field [current-room room0])
      (define (on-state)
        (start-facet im-on
          (assert (avatar-location (! current-room)))
          (on (message (move-cmd _))
              (stop im-on (realize! (turn-off))))))

      (define (off-state)
        (start-facet im-off
          (on (message (move-cmd _))
              (stop im-off (realize! (turn-on))))))

      (on start (on-state))

      (on (realize (turn-on)) (on-state))
      (on (realize (turn-off)) (off-state)))))

(module+ test
  (define-type-alias IO-types
    (Message MoveCmd))

  (verify-actors (Or (Always (And (Implies (And (A AvatarLocation)
                                                (M MoveCmd))
                                           (Eventually (Not (A AvatarLocation))))
                                  (Eventually (And (A AvatarLocation)
                                                   (M MoveCmd)))))
                     (Eventually (Always (Not (M MoveCmd)))))
    #:IO IO-types
    avatar-impl))
