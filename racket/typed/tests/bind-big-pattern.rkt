#lang typed/syndicate

(require rackunit/turnstile)

(define-constructor* (go [t : Int] [w : String]))

(check-type
 (on (asserted ($ g (go 5 "home")))
     (add1 (go-t g))
     (string->symbol (go-w g)))
 : ★/t)

(check-type
 (on (message ($ g (go $t _)))
     (add1 (go-t g))
     (add1 t)
     (string->symbol (go-w g)))
 : ★/t)

(typecheck-fail (on (asserted ($ g (go $t _))) #f)
                #:with-msg "Nested bindings only supported in message events")
