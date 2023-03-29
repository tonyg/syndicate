#lang typed/syndicate

(require rackunit/turnstile)

(lambda ()
  (react
    (on start
        (stop this-facet))))

(typecheck-fail this-facet
                #:with-msg "use of this-facet outside of a facet")
(typecheck-fail (start-facet named
                  (lambda () (stop this-facet)))
                #:with-msg "use of this-facet outside of a facet")
(typecheck-fail (stop-when #t)
                #:with-msg "Not in a context with a known parent facet")
(lambda ()
  (during/spawn (tuple $x:Int)
    (displayln x)
    (stop-when (message (tuple)))))
