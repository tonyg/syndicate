#lang typed/syndicate

(require rackunit/turnstile)

(typecheck-fail (spawn #:type (U)
                       (start-facet x
                                    (assert 42))))
