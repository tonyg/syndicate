#lang typed/syndicate/roles

(require rackunit/turnstile)

(check-type (or #f #t)
            : Bool
            ⇒ #t)

(check-type (and #t #f)
            : Bool
            ⇒ #f)

(check-type (or)
            : Bool
            ⇒ #f)

(check-type (and)
            : Bool
            ⇒ #t)

(check-type (or #f #f #f #f #f #t)
            : Bool
            ⇒ #t)

(check-type (and #t #t #t #t #t #f)
            : Bool
            ⇒ #f)
