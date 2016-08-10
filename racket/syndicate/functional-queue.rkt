#lang racket/base

(provide make-queue
	 queue?
	 enqueue
	 enqueue-all
	 queue-prepare-for-dequeue
	 dequeue
	 list->queue
	 queue->list
	 queue-length
	 queue-empty?
	 queue-append
	 queue-append-list
	 queue-extract
         queue-filter
         queue-remove
         queue-partition)

(require (only-in racket/list partition))

(struct queue (head tail) #:transparent)

(define (make-queue)
  (queue '() '()))

(define (enqueue q v)
  (queue (queue-head q)
	 (cons v (queue-tail q))))

(define (enqueue-all q v)
  (queue (queue-head q)
	 (append (reverse v) (queue-tail q))))

(define (queue-prepare-for-dequeue q)
  (if (null? (queue-head q))
      (queue (reverse (queue-tail q)) '())
      q))

(define (dequeue q)
  (let ((q1 (queue-prepare-for-dequeue q)))
    (values (car (queue-head q1))
	    (queue (cdr (queue-head q1)) (queue-tail q1)))))

(define (list->queue xs)
  (queue xs '()))

(define (queue->list q)
  (append (queue-head q) (reverse (queue-tail q))))

(define (queue-length q)
  (+ (length (queue-head q))
     (length (queue-tail q))))

(define (queue-empty? q)
  (and (null? (queue-head q))
       (null? (queue-tail q))))

(define (queue-append q1 q2)
  (queue (append (queue-head q1)
		 (reverse (queue-tail q1))
		 (queue-head q2))
	 (queue-tail q2)))

(define (queue-append-list q1 xs)
  (queue (queue-head q1)
	 (append (reverse xs) (queue-tail q1))))

(define (queue-extract q predicate [default-value #f])
  (let search-head ((head (queue-head q))
		    (rejected-head-rev '()))
    (cond
     ((null? head) (let search-tail ((tail (reverse (queue-tail q)))
				     (rejected-tail-rev '()))
		     (cond
		      ((null? tail) (values default-value q))
		      ((predicate (car tail)) (values (car tail)
						      (queue (queue-head q)
							     (append (reverse (cdr tail))
								     rejected-tail-rev))))
		      (else (search-tail (cdr tail) (cons (car tail) rejected-tail-rev))))))
     ((predicate (car head)) (values (car head)
				     (queue (append (reverse rejected-head-rev)
						    (cdr head))
					    (queue-tail q))))
     (else (search-head (cdr head) (cons (car head) rejected-head-rev))))))

(define (queue-filter pred q)
  (queue (filter pred (queue-head q))
         (filter pred (queue-tail q))))

(define (queue-remove item q)
  (list->queue (remove item (queue->list q))))

(define (queue-partition pred q)
  (define-values (head-t head-f) (partition pred (queue-head q)))
  (define-values (tail-t tail-f) (partition pred (queue-tail q)))
  (values (queue head-t tail-t)
          (queue head-f tail-f)))
