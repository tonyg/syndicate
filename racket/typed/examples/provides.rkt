#lang typed/syndicate

(provide a-fun)

(define (a-fun [x : Int] -> Int)
  (+ x 1))

#;(a-fun 5)
