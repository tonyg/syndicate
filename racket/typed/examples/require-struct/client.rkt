#lang typed/syndicate

(require-struct msg #:as Msg
                #:from "driver.rkt")

(define m (msg 1 "hi"))

(msg-in m)
(msg-out m)

(match m
  [(msg (bind x Int) discard)
   (displayln x)])

;; error: msg/checked: arity mismatch
#;(msg 1 2 3)
