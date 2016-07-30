#lang syndicate-monolithic
;; Simple mutable box and count-to-infinity box client.

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(spawn (lambda (e current-value)
         (match e
           [(message (set-box new-value))
            (log-info "box: taking on new-value ~v" new-value)
            (transition new-value (scn/union (subscription (set-box ?))
                                             (assertion (box-state new-value))))]
           [_ #f]))
       0
       (scn/union (subscription (set-box ?))
                  (assertion (box-state 0))))

(spawn (lambda (e s)
         (match e
           [(scn assertions)
            (transition s (for/list [(v (project-assertions assertions (box-state (?!))))]
                            (log-info "client: learned that box's value is now ~v" v)
                            (message (set-box (+ v 1)))))]
           [_ #f]))
       (void)
       (scn (subscription (box-state ?))))
