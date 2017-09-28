#lang syndicate

(assertion-struct cell (id value))
(message-struct create-cell (id value))
(message-struct update-cell (id value))
(message-struct delete-cell (id))

(define (spawn-cell initial-value)
  (define id (gensym 'cell))
  (send! (create-cell id initial-value))
  id)

(spawn #:name 'cell-factory
  (on (message (create-cell $id $initial-value))
      (spawn #:name (list 'cell id)
        (field [value initial-value])
        (assert (cell id (value)))
        (on (message (update-cell id $new-value)) (value new-value))
        (stop-when (message (delete-cell id))))))

(define (spawn-cell-monitor id)
  (spawn #:name (list 'cell-monitor id)
    (on (asserted (cell id $value))
        (printf "Cell ~a updated to: ~a\n" id value))
    (on (retracted (cell id _))
        (printf "Cell ~a deleted\n" id))))

(define (read-cell id)
  (flush!) ;; this is important! else previous writes remain buffered after the first read
  (react/suspend (k)
                 (stop-when (asserted (cell id $value)) (k value))))

(spawn* #:name 'main-actor
        (define id (spawn-cell 123))
        (spawn-cell-monitor id)
        (send! (update-cell id (+ (read-cell id) 1)))
        (send! (update-cell id (+ (read-cell id) 1)))
        (send! (update-cell id (+ (read-cell id) 1)))
        (send! (delete-cell id)))
