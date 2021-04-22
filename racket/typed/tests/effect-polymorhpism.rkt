#lang typed/syndicate

(require rackunit/turnstile)

(define (∀ (ρ) (assert-something! [p : (proc → ★/t #:endpoints (ρ))]))
  (p))

(define (test-fun)
  (call/inst assert-something! (lambda () (assert 5))))


(check-type test-fun : (proc → ★/t #:endpoints ((Shares Int))))

(define (test-call/inst-insertion)
  (assert-something! (lambda () (assert 5))))

(check-type test-call/inst-insertion : (proc → ★/t #:endpoints ((Shares Int))))
