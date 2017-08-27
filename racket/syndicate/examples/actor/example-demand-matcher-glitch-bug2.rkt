#lang syndicate/actor
;; Example showing the consequences of not honouring the requirement
;; of the current LLL demand-matcher that supply tasks must *reliably*
;; terminate when their demand is not present. In this case, demand
;; changes too quickly: it exists for long enough to start the task,
;; but is withdrawn before the task itself has a chance to detect it.
;; Because the task (as currently implemented) does not use the "learn
;; negative knowledge" pattern to detect the *absence* of some
;; assertion, it does not terminate as it is supposed to.
;;
;; Demonstrates that the fix in commit 2a0197b isn't in general
;; sufficient.

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)

(define X (advertise (observe (tcp-channel ? (tcp-listener 6000) ?))))

(spawn* #:name 'strobe
        (assert! X)
        (printf "Asserted ~v\n" X)
        ;; (begin (send! 'dummy)
        ;;        (retract! X)
        ;;        (printf "Retracted ~v\n" X))
        (printf "Terminating.\n"))
