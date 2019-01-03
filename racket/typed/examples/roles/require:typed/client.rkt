#lang typed/syndicate/roles

(require/typed "lib.rkt" [x : Int])

(displayln (+ x 1))