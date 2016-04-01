#lang syndicate
;; Simple mutable box and count-to-infinity box client.

(require syndicate/actor)

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(actor (forever #:collect [(current-value 0)]
                (assert (box-state current-value))
                (on (message (set-box $new-value))
                    (log-info "box: taking on new-value ~v" new-value)
                    new-value)))

(actor (forever (on (asserted (box-state $v))
                    (log-info "client: learned that box's value is now ~v" v)
                    (send! (set-box (+ v 1))))))
