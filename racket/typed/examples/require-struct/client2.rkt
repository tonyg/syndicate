#lang typed/syndicate

;; using different syntax than "client.rkt"

(require/typed "driver.rkt" [#:struct msg])

(define m : (MsgT Int String) (msg 1 "hi"))

(msg-in m)
(msg-out m)

(match m
  [(msg (bind x Int) _)
   (displayln x)])

;; error: msg/checked: arity mismatch
#;(msg 1 2 3)
