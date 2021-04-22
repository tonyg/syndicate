#lang typed/syndicate

(require rackunit/turnstile)

;; TODO - currently fails with a racket error. Is that OK?
;; (ideally, this would fail with a better error message)
(typecheck-fail
 (let ([x (define y 5)])
   (add1 1)))
