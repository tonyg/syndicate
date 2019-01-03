#lang typed/syndicate/roles

(require-struct msg #:as Msg
                #:from "driver.rkt")

(define m (msg 1 "hi"))

(match m
  [(msg (bind x Int) discard)
   (displayln x)])

;; error: msg/checked: arity mismatch
#;(msg 1 2 3)