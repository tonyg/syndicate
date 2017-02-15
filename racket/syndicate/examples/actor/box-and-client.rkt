#lang syndicate/actor
;; Simple mutable box and count-to-infinity box client.

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(spawn (field [current-value 0])
       (assert (box-state (current-value)))
       (stop-when (rising-edge (= (current-value) 10))
                  (log-info "box: terminating"))
       (on (message (set-box $new-value))
           (log-info "box: taking on new-value ~v" new-value)
           (current-value new-value)))

(spawn (stop-when (retracted (observe (set-box _)))
                  (log-info "client: box has gone"))
       (on (asserted (box-state $v))
           (log-info "client: learned that box's value is now ~v" v)
           (send! (set-box (+ v 1)))))
