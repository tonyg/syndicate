#lang typed/syndicate

(require rackunit/turnstile)

(lambda ()
  (assert (tuple ★)))

(lambda ()
  (spawn
    (react
      (assert (tuple ★))
      (on (asserted (tuple ★))
          #f))))

(typecheck-fail
 (spawn
   (react
     (assert (tuple ★))
     (on (asserted (tuple $x:Int))
         #f)))
 #:with-msg "Not prepared to handle inputs")
