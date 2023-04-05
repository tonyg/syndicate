#lang typed/syndicate

(require/typed "untyped-lib.rkt"
  [#:struct chicken [eggs : Int]]
  [#:alias Chickens (List Chicken)]
  [#:struct roost [chickens : Chickens]])
