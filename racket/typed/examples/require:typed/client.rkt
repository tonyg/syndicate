#lang typed/syndicate

(require/typed "lib.rkt" [x : Int])

(displayln (+ x 1))
