#lang typed/syndicate/roles

(provide activate!
         later-than
         LaterThanT
         LaterThan
         TimeStateDriver)

(require-struct later-than
                #:as LaterThanT
                #:from syndicate/drivers/timestate)

(define-type-alias LaterThan (LaterThanT Int))

(define-type-alias TimeStateDriver
  (U LaterThan
     (Observe (LaterThanT ★/t))))

;; TODO ignoring other driver underneath it

(require/typed (submod syndicate/drivers/timestate syndicate-main)
  [activate! : (proc → ⊥ #:spawns ((Actor TimeStateDriver)))])
