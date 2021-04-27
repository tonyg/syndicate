#lang typed/syndicate

(require/typed "lib.rkt"
  [#:opaque Vec]
  [ones : Vec]
  [vec+ : (→fn Vec Vec Vec)])

(vec+ ones ones)
