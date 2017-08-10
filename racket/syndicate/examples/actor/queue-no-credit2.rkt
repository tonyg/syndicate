#lang syndicate
;; A Queue with no flow control.

(require racket/set)
(require syndicate/functional-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queue Protocol

;; Assertion. Scopes flow from source to target.
(struct subscription (source target) #:prefab)

;; Message. Delivery from source to target.
(struct delivery (source target body) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metrics Protocol

;; Assertion. Describes some attribute of monitoringish interest.
(struct metric (key value) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;; (Fieldof (Queueof X)) -> X
;; EFFECT: Changes f.
;; EFFECT: Error if f contains the empty queue.
(define (deq! f)
  (define-values (item remainder) (dequeue (f)))
  (f remainder)
  item)

;; (Fieldof (Queueof X)) X -> Void
;; EFFECT: Changes f.
(define (enq! f v)
  (f (enqueue (f) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queue Implementation

;; EFFECT: Spawn a queue process named `queue-id`.
(define (spawn-queue queue-id)
  (spawn #:name (list 'queue queue-id)
         (field [waiters (make-queue)])
         (field [messages (make-queue)])

         (on (asserted (subscription queue-id $who)) (enq! waiters who))
         (on (retracted (subscription queue-id $who)) (waiters (queue-remove who (waiters))))
         (on (message (delivery $who queue-id $body)) (enq! messages body))

         (begin/dataflow
           (when (and (not (queue-empty? (waiters)))
                      (not (queue-empty? (messages))))
             (define who (deq! waiters))
             (define msg (deq! messages))
             (log-info "~a: sending ~a message ~a" queue-id who msg)
             (send! (delivery queue-id who msg))
             (enq! waiters who)))

         (assert (metric (list 'subscriber-count queue-id) (queue-length (waiters))))
         (assert (metric (list 'backlog queue-id) (queue-length (messages))))

         ;;------------------------------------------------------------

         (local-require (submod syndicate/actor priorities))
         (begin/dataflow #:priority *idle-priority* ;; Check invariants
                         (define has-waiters? (not (queue-empty? (waiters))))
                         (define has-messages? (not (queue-empty? (messages))))
                         (unless (and (or (not has-waiters?) (not has-messages?))
                                      (or (not has-messages?) (not has-waiters?)))
                           (error 'queue
                                  "~a: invariant violated: ~v"
                                  queue-id
                                  `((has-waiters? ,has-waiters?)
                                    (has-messages? ,has-messages?)
                                    (waiters ,(queue->list (waiters)))
                                    (messages ,(queue->list (messages)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example

(define (spawn-consumer consumer-id #:variant [variant 'normal])
  (spawn #:name (list 'consumer consumer-id)
         (assert (subscription 'q consumer-id))
         (on (message (delivery 'q consumer-id $body))
             (log-info "Consumer ~a got: ~a" consumer-id body)
             (when (eq? variant 'crashy)
               (error consumer-id
                      "Hark, canst thou hear me? I will play the swan / and die in music.")))))

(spawn (define/query-hash metrics (metric $k $v) k v)
       (begin/dataflow (log-info "  ~a" (hash->list (metrics)))))

(spawn-queue 'q)
(spawn-consumer 'c1)
(spawn-consumer 'c2 #:variant 'crashy)
(spawn-consumer 'c3)

(spawn* (until (asserted (observe (delivery _ 'q _))))
        (for ((n (in-range 10)))
          (send! (delivery #f 'q n))
          (when (odd? n) (flush!))
          ))
