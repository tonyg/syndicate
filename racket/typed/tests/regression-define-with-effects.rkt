#lang typed/syndicate/roles

(require rackunit/turnstile)

(check-type
 (let ()
  (field [boo Int 0])
  (define x (begin (send! "hi") 5))
  ;; relying on `set` not allowing effects for this to be a good test
  (set! boo x)
  3)
 : Int)

;; Used to get the error:
; <pkgs>/syndicate/typed/tests/define-with-effects.rkt:10.2: set!: expression not allowed to have effects
;   at: (set! boo x)
;   in: (set! boo x)
