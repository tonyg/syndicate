#lang typed/syndicate

(require rackunit/turnstile)

(lambda ()
  (react
    (on start
        (stop this-facet))))

(typecheck-fail this-facet)
(typecheck-fail (start-facet named
                  (lambda () (stop this-facet))))
