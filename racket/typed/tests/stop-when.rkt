#lang typed/syndicate

(require rackunit/turnstile)

(lambda ()
  (start-facet boop
    (stop-when (message (tuple)))))

(lambda ()
  (start-facet boop
    (stop-when #:when #t
               (message (tuple))
               (+ 1 1))))

(lambda ()
  (react (stop-when (message (tuple)))))

(typecheck-fail (react
                  (define (f) (stop-when (message (tuple))))
                  #f))
