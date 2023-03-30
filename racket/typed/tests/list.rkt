#lang typed/syndicate

(require rackunit/turnstile)

(check-type (map add1 (list 1 2 3))
            : (List Int))

(typecheck-fail (map add1 (list (list 1 2 3))))

(check-type (map (inst first Int) (list (list 1 2 3)
                                        (list 4 5 6)))
            : (List Int))
