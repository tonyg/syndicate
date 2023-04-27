#lang typed/syndicate

(require rackunit/turnstile)

(module+ test
  (typecheck-fail (verify-actors TT)
                  #:with-msg "verify-actors: expected more terms"))
