#lang typed/syndicate/roles

(require rackunit/turnstile)

(check-type (for/list ([x (for/list ([y (list 1 2 3)])
                            y)])
              x)
            : (List Int)
            ⇒ (list 1 2 3))
