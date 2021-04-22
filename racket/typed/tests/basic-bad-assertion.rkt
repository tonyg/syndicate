#lang typed/syndicate

(require rackunit/turnstile)

(typecheck-fail (spawn (U)
                       (start-facet x
                                    (assert 42))))
