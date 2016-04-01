#lang syndicate
;; Simple mutable box and count-to-infinity box client.

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(spawn (lambda (e current-value)
         (match-event e
           [(message (set-box new-value))
            (log-info "box: taking on new-value ~v" new-value)
            (transition new-value (patch-seq (retract (box-state current-value))
                                             (assert (box-state new-value))))]))
       0
       (patch-seq (sub (set-box ?))
                  (assert (box-state 0))))

(spawn (lambda (e s)
         (match-event e
           [(patch added removed)
            (transition s (for-trie/list ([(box-state $v) added])
                            (log-info "client: learned that box's value is now ~v" v)
                            (message (set-box (+ v 1)))))]))
       (void)
       (patch-seq (sub (box-state ?))))
