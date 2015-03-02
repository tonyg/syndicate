#lang racket/base
;; Poor-man's hash consing.

(provide canonicalize)

(define canonical-values (make-weak-hash))

(define sentinel (cons #f #f))

(define (canonicalize val)
  (define b (hash-ref canonical-values
		      val
		      (lambda ()
			(define new-b (make-weak-box val))
			(hash-set! canonical-values val new-b)
			#f)))
  (if (not b)
      (canonicalize val)
      (let ((v (weak-box-value b sentinel)))
	(if (eq? v sentinel) (canonicalize val) v))))

(module+ test
  (require rackunit)
  
  (define v1 (canonicalize (cons 1 2)))
  
  (let ((v2 (canonicalize (cons 1 2))))
    (check-eq? v1 v2))
  
  (collect-garbage)
  (check-equal? (hash-count canonical-values) 1)
  
  (let ((v2 (canonicalize (cons 1 2))))
    (check-eq? v1 v2))
  
  (set! v1 (canonicalize (cons 1 2)))
  
  (collect-garbage)
  (check-equal? (hash-count canonical-values) 1)
  
  (let ((v2 (canonicalize (cons 1 2))))
    (check-eq? v1 v2))
  
  (set! v1 #f)
  
  (collect-garbage)
  (check-equal? (hash-count canonical-values) 0))
