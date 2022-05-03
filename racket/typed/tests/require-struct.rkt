#lang typed/syndicate

(require rackunit/turnstile)

(require/typed "struct-provider.rkt"
  [#:struct donkey [weight : Int] [grey? : Bool]])

(check-type (donkey 5 #t)
            : (DonkeyT Int Bool))

(check-type (donkey-grey? (donkey 5 #t))
            : Bool)
