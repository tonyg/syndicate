#lang racket/base
;; Continuation-mark-based explicitly-scoped stores

(provide (struct-out store)
         make-store
         with-store
         store-box
         store-ref
         store-set!)

(struct store (mark-key)
  #:property prop:procedure
  (case-lambda
    [(s) (store-ref s)]
    [(s v) (store-set! s v)]))

(define (make-store)
  (store (make-continuation-mark-key (gensym 'store))))

(define (store-box s)
  (or (continuation-mark-set-first #f (store-mark-key s))
      (error 'store-box
             "Attempt to access store that is not currently in scope")))

(define (store-ref s)
  (unbox (store-box s)))

(define (store-set! s v)
  (set-box! (store-box s) v))

(define-syntax with-store
  (syntax-rules ()
    [(_ [] body ...)
     (let () body ...)]
    [(_ [(st initial-val) more ...] body ...)
     (let ((s st))
       (with-continuation-mark (store-mark-key s) (box initial-val)
         (with-store [more ...] body ...)))]))

(module+ test
  (require rackunit)

  (define p (make-parameter 123))

  (define t (make-continuation-prompt-tag 'store-test))

  (define s1 (make-store))
  (define s2 (make-store))

  (with-store [(s1 's1)]
    (check-equal? (s1) 's1)
    (s1 'b)
    (check-equal? (s1) 'b))

  (with-store [(s1 's1)
               (s2 's2)]
    (check-equal? (s1) 's1)
    (check-equal? (s2) 's2)
    (s1 'b)
    (s2 'b)
    (with-store [(s1 'c)]
      (check-equal? (s1) 'c)
      (check-equal? (s2) 'b)
      (s1 'd)
      (s2 'd)
      (check-equal? (s1) 'd)
      (check-equal? (s2) 'd))
    (check-equal? (s1) 'b)
    (check-equal? (s2) 'd))

  (define (s1-push! x)
    (s1 (cons x (s1))))

  (with-store [(s1 '())]
    (define k
      (call-with-continuation-prompt
       (lambda ()
         (s1-push! 'a)
         (s1-push! (p))
         (parameterize ((p 234))
           (s1-push! (p))
           (s1-push! (call-with-composable-continuation
                      (lambda (k)
                        (abort-current-continuation
                         t
                         (lambda ()
                           (s1-push! 'x)
                           k)))
                      t))
           (s1-push! 'b)
           (s1-push! (p)))
         (s1-push! (p))
         (s1-push! 'c))
       t))
    (s1-push! 'y)
    (k 99)
    (s1-push! 'd)
    (check-equal? (reverse (s1)) '(a 123 234 x y 99 b 234 123 c d))))
