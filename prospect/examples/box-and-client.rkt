#lang prospect
;; Simple mutable box and count-to-infinity box client.

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(spawn (lambda (e current-value)
         (match e
           [(message (set-box new-value))
            (transition new-value (patch-seq (retract (box-state current-value))
                                             (assert (box-state new-value))))]
           [_ #f]))
       0
       (sub (set-box ?))
       (assert (box-state 0)))

(spawn (lambda (e s)
         (match e
           [(patch added removed)
            (transition s (for/list [(v (matcher-project/set/single
                                         added
                                         (compile-projection (box-state (?!)))))]
                            (message (set-box (+ v 1)))))]
           [_ #f]))
       (void)
       (sub (box-state ?)))
