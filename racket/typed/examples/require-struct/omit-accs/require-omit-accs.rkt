#lang typed/syndicate/roles

(require-struct egg #:as Egg #:from "lib.rkt" #:omit-accs)

(define e (egg 5 "Sun"))

(match e
  [(egg $sz $d)
   (displayln sz)
   (displayln d)])

(require-struct chicken #:as Chicken #:from "lib.rkt" #:omit-accs)

(define c (chicken (list e e e)))

(match c
  [(chicken $eggs)
   (displayln eggs)])
