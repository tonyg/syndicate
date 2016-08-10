#lang syndicate/actor
;; Counting-semaphore-like mutex, and dining philosophers.

(require racket/set)
(require syndicate/functional-queue)

(require/activate syndicate/drivers/timer)

(struct lease-request (resource-id request-id) #:prefab)
(struct lease-assignment (resource-id request-id) #:prefab)
(struct resource-status (resource-id waiter-count) #:prefab)

(define (spawn-resource resource-id total-available-leases)
  (actor
   (react (field [waiters (make-queue)]
                 [free-lease-count total-available-leases])

          (begin/dataflow (log-info "~as available: ~a" resource-id (free-lease-count)))

          (on (asserted (lease-request resource-id $w))
              (cond [(positive? (free-lease-count))
                     (assert! (lease-assignment resource-id w))
                     (free-lease-count (- (free-lease-count) 1))]
                    [else
                     (waiters (enqueue (waiters) w))]))

          (on (retracted (lease-request resource-id $w))
              (waiters (queue-filter (lambda (x) (not (equal? w x))) (waiters)))
              (retract! (lease-assignment resource-id w)))

          (on (retracted (lease-assignment resource-id $w))
              (cond [(queue-empty? (waiters))
                     (free-lease-count (+ (free-lease-count) 1))]
                    [else
                     (define-values (w remainder) (dequeue (waiters)))
                     (assert! (lease-assignment resource-id w))
                     (waiters remainder)])))))

;;---------------------------------------------------------------------------

(struct philosopher-status (name status) #:prefab)

(actor (react (define/query-hash-set thinkers (philosopher-status $who $status) status who)
              (begin/dataflow
                (log-info "~a" (for/list (((status names) (in-hash (thinkers))))
                                 (format "~a: ~a" status (set->list names)))))))

(define (philosopher name)
  (actor
   (react (field [status 'starting])
          (assert (philosopher-status name (status)))

          (stop-when (rising-edge (eq? (status) 'inspired)))

          (on-start
           (let loop ()
             (define thinking-duration (* (random) 4))
             (log-info "~a thinks for ~a seconds" name thinking-duration)
             (status 'thinking)
             (until (message (timer-expired name _))
                    (on-start (send! (set-timer name (* thinking-duration 1000.0) 'relative))))
             (if (> (random) 0.95)
                 (begin
                   (log-info "~a stops thinking, leaps up, shouts \"EUREKA!\", and leaves.\n" name)
                   (status 'inspired))
                 (begin
                   (log-info "~a stops thinking, and waits for a fork" name)
                   (status 'waiting)
                   (react (assert (lease-request 'fork name))
                          (on (asserted (lease-assignment 'fork name))
                              (status 'eating)
                              (log-info "~a claims a fork" name)
                              (define eating-duration (* (random) 4))
                              (log-info "~a is eating for ~a seconds" name eating-duration)
                              (send! (set-timer name (* eating-duration 1000.0) 'relative)))
                          (stop-when (message (timer-expired name _))
                                     (log-info "~a finishes eating and puts down the fork" name)
                                     (loop))))))))))

(spawn-resource 'fork 2)
(philosopher 'Socrates)
(philosopher 'Kierkegaard)
(philosopher 'Wittgenstein)
(philosopher 'Descartes)
