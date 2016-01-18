#lang prospect
;; Simple mutable box and count-to-infinity box client.

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(spawn (lambda (e current-value)
         (match e
           [(message (set-box new-value))
            (log-info "box: taking on new-value ~v" new-value)
            (transition new-value (patch-seq (retract (box-state current-value))
                                             (assert (box-state new-value))))]
           [_ #f]))
       0
       (patch-seq (sub (set-box ?))
                  (assert (box-state 0))))

(spawn (lambda (e s)
         (match e
           [(patch added removed)
            (transition s (for/list [(v (project-assertions added (box-state (?!))))]
                            (log-info "client: learned that box's value is now ~v" v)
                            (message (set-box (+ v 1)))))]
           [_ #f]))
       (void)
       (patch-seq (sub (box-state ?))))
