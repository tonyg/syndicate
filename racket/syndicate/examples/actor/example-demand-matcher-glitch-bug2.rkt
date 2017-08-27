#lang syndicate/actor
;; Example showing an interesting design flaw (?) (bug?) in the
;; old-school LLL demand-matcher when demand changes too quickly. The
;; port 6000 server is started, but by the time it starts monitoring
;; demand for its services, the demand is already gone, replaced with
;; demand for port 5999. This causes connections to be accepted on
;; port 6000 going nowhere.
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
