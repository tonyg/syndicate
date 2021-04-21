#lang racket/base

(provide run/timeout
         define/timeout)

(require racket/engine)

;; (-> A) Real -> (U A Engine)
;; run the given thunk in an engine for 'fuel' milliseconds
;; if the engine completes, returns the result, otherwise the engine itself
(define (run/timeout tnk [fuel 1000])
  (define e (engine (lambda (p) (tnk))))
  (define r (engine-run fuel e))
  (if r (engine-result e) e))

(define-syntax-rule (define/timeout x e)
  (define x (run/timeout (lambda () e))))
