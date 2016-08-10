#lang syndicate/actor
;; A Queue with credit-based flow control.

(require racket/set)
(require syndicate/functional-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queue Protocol

;; Assertion. Scopes flow from source to target.
(struct subscription (source target) #:prefab)

;; Message. Increases flow-control-limit on flow from source to target.
(struct credit (source target amount) #:prefab)

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
  (actor #:name (list 'queue queue-id)
   (react (define/query-hash credits (subscription queue-id $who) who 0) ;; Start with no credit
          (field [waiters (make-queue)])
          (field [messages (make-queue)])

          (during (subscription queue-id $who)
                  (assert (metric (list 'credit queue-id who) (hash-ref (credits) who 0))))
          (assert (metric (list 'backlog queue-id) (queue-length (messages))))

          (on (message (credit queue-id $who $amount))
              (define old-credit (hash-ref (credits) who #f))
              (when old-credit
                (credits (hash-set (credits) who (+ amount old-credit)))
                (when (zero? old-credit) (enq! waiters who))))

          (on (message (delivery $who queue-id $body))
              (send! (credit who queue-id 1))
              (enq! messages body))

          (begin/dataflow
            (when (and (not (queue-empty? (waiters)))
                       (not (queue-empty? (messages))))
              (define who (deq! waiters))
              (define old-credit (hash-ref (credits) who 0))
              (when (positive? old-credit)
                (define new-credit (- old-credit 1))
                (credits (hash-set (credits) who new-credit))
                (when (positive? new-credit) (enq! waiters who))
                (define msg (deq! messages))
                (log-info "~a: sending ~a message ~a" queue-id who msg)
                (send! (delivery queue-id who msg)))))

          ;;------------------------------------------------------------

          (local-require (submod syndicate/actor priorities))
          (begin/dataflow #:priority *idle-priority* ;; Check invariants
            (define has-waiters? (not (queue-empty? (waiters))))
            (define has-messages? (not (queue-empty? (messages))))
            (define total-credits (for/sum ((v (in-hash-values (credits)))) v))
            (unless (and (or (not has-messages?) (zero? total-credits))
                         (or (not has-waiters?) (not has-messages?))
                         (equal? has-waiters? (positive? total-credits)))
              (error 'queue
                     "~a: invariant violated: ~v"
                     queue-id
                     `((has-waiters? ,has-waiters?)
                       (has-messages? ,has-messages?)
                       (total-credits ,total-credits)
                       (waiters ,(queue->list (waiters)))
                       (messages ,(queue->list (messages)))
                       (credits ,(hash->list (credits))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example

(define (spawn-consumer consumer-id initial-credit #:variant [variant 'normal])
  (actor #:name (list 'consumer consumer-id)
   (react (assert (subscription 'q consumer-id))
          (on-start (send! (credit 'q consumer-id initial-credit)))
          (on (message (delivery 'q consumer-id $body))
              (log-info "Consumer ~a got: ~a" consumer-id body)
              (case variant
                [(normal)
                 (send! (credit 'q consumer-id 1))]
                [(crashy)
                 (error consumer-id
                        "Hark, canst thou hear me? I will play the swan / and die in music.")]
                [(overloaded) ;; don't issue credit
                 (void)])))))

(actor (react (define/query-hash metrics (metric $k $v) k v)
              (begin/dataflow (log-info "  ~a" (hash->list (metrics))))))

(spawn-queue 'q)
(spawn-consumer 'c1 2)
(spawn-consumer 'c2 2 #:variant 'crashy)
(spawn-consumer 'c3 3 #:variant 'overloaded)

(actor (until (asserted (observe (delivery _ 'q _))))
       (for ((n (in-range 10)))
         (send! (delivery #f 'q n))
         ;; (flush!)
         ))
