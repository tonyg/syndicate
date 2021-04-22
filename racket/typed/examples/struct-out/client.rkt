#lang typed/syndicate/roles

(require "typed-out.rkt")

(define c : (Cow Int) (cow 5))

(cow-moos c)
