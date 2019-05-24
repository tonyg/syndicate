#lang typed/syndicate/roles

(require rackunit/turnstile)

(define-type-alias τc
  (U (Tuple Int)
     (Observe (Tuple ★/t))))

;; I actually think this is OK, since elaborating the pattern inserts a type
;; that will still be checked by `project-safe?`

(check-type (lambda ()
              (spawn τc
                (begin
                  (define (on!)
                    (on (asserted (tuple $x))
                        #f))
                (start-facet x
                  (on!)))))
            : (proc -> ★/t #:spawns ((Actor τc))))
