#lang typed/syndicate

(require rackunit/turnstile)

(define (∀ (ρ) (assert-something! [p : (proc → ★/t #:effects (ρ))]))
  (p))

(define (test-fun)
  (call/inst assert-something! (lambda () (assert 5))))


(check-type test-fun : (proc → ★/t #:effects ((Shares Int))))

(define (test-call/inst-insertion)
  (assert-something! (lambda () (assert 5))))

(check-type test-call/inst-insertion : (proc → ★/t #:effects ((Shares Int))))

(define (∀ (ρ) (start-something! [p : (proc → ★/t #:effects (ρ))]))
  (p))

(define (test-call-start-something)
  (start-something! (lambda () (start-facet x (assert 5)))))
