#lang typed/syndicate

(require rackunit/turnstile)

(lambda ()
  (spawn #:name 'an-actor
         (react #f)))

(lambda ()
  (spawn #:name (list 'x 'y 'z)
         (react #f)))

(lambda ([x : Int])
  (spawn #:name (add1 x)
         (react #f)))

(typecheck-fail (spawn #:name (assert (tuple)))
                #:with-msg "must be pure")

(lambda ()
  (during/spawn (tuple)
    #:name (list 'x 'y 'z)
    #f))

(lambda ()
  (during/spawn (tuple $x:Int)
    #:name (list x)
    #f))
