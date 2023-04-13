#lang typed/syndicate

(require rackunit/turnstile)

(define-constructor* (session [id : Int] v))

(check-type (session 5 "hi")
            : (Session String))

(check-type (session "hi" 42)
            : (SessionT String Int))

(lambda ()
  (on (asserted (session $id ★))
      (add1 id)))
