#lang racket/base

(provide on-claim)

(require syndicate/monolithic)
(require syndicate/drivers/timer)

;; (Trie (Option (Setof (Listof Value))) ... -> (Option (Constreeof Action)))
;;    Trie Projection ...
;; -> Action
;; Spawns a process that observes the given projections. Any time the
;; environment's interests change in a relevant way, calls
;; check-and-maybe-spawn-fn with the aggregate interests and the
;; projection results. If check-and-maybe-spawn-fn returns #f,
;; continues to wait; otherwise, takes the action(s) returned, and
;; quits.
(define (on-claim #:timeout-msec [timeout-msec #f]
                  #:on-timeout [timeout-handler (lambda () '())]
                  #:name [name #f]
                  check-and-maybe-spawn-fn
                  base-interests
                  . projections)
  (define timer-id (gensym 'on-claim))
  (define (on-claim-handler e state)
    (match e
      [(scn new-aggregate)
       (define projection-results
         (map (lambda (p) (trie-project/set #:take (projection-arity p) new-aggregate p))
              projections))
       (define maybe-spawn (apply check-and-maybe-spawn-fn
                                  new-aggregate
                                  projection-results))
       (if maybe-spawn
           (quit maybe-spawn)
           #f)]
      [(message (timer-expired (== timer-id) _))
       (quit (timeout-handler))]
      [_ #f]))
  (list
   (when timeout-msec (message (set-timer timer-id timeout-msec 'relative)))
   (spawn #:name name
          on-claim-handler
          (void)
          (scn/union base-interests
                     (assertion-set-union*
                      (map (lambda (p) (subscription (projection->pattern p))) projections))
                     (subscription (timer-expired timer-id ?))))))
