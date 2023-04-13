#lang typed/syndicate

(require rackunit/turnstile)

(require/typed "struct-provider.rkt"
  [#:struct donkey [weight : Int] grey?])

(check-type (donkey 5 #t)
            : (Donkey Bool))

(check-type (donkey "boo" #t)
            : (DonkeyT String Bool))

(check-type (donkey-grey? (donkey "boo" #t))
            : Bool)
