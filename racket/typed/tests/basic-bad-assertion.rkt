#lang typed/syndicate/roles

(require rackunit/turnstile)

(typecheck-fail (spawn (U)
                       (start-facet x
                                    (assert 42))))
