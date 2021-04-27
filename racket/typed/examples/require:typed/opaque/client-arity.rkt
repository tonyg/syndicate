#lang typed/syndicate

(require/typed "lib.rkt"
  [#:opaque Vec #:arity = 3]
  [ones : (Vec Int Int Int)]
  [vec+ : (→fn (Vec Int Int Int) (Vec Int Int Int) (Vec Int Int Int))])

(vec+ ones ones)
