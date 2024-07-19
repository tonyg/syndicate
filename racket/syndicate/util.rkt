#lang syndicate

(provide capture-spawn-actions)

(require "store.rkt"
         (submod "actor.rkt" implementation-details))

(define (capture-spawn-actions loc thunk)
  (call-with-syndicate-effects
   (lambda ()
     (with-store [(current-pending-actions '())
                  (current-pending-patch patch-empty)
                  (current-action-transformer values)]
       (call-with-values thunk
                         (lambda results
                           (ensure-spawn-actions! loc (cons results (current-pending-actions)))))))))

(define (ensure-spawn-actions! loc acts)
  (check-spawn-actions! acts
                        (lambda (act) (raise-argument-error loc
                                                            "actor creation action"
                                                            act))))
