#lang typed/syndicate

(require rackunit/turnstile)

(check-type (hash) : (Hash (U) (U)))

(check-type (hash 1 2) : (Hash Int Int))

(check-type (hash "greetings" 8) : (Hash String Int))

(check-type (hash "smelly" 0
                  "feet" 18
                  "robust" 9)
            : (Hash String Int))

(check-type (hash "smelly" 0
                  "feet" "grosss"
                  "robust" #t)
            : (Hash String (U Int String Bool)))

(define a-hash
  (hash "smelly" 0
        "feet" 18
        "robust" 9))

(define hash-ref/inst (inst hash-ref String Int))

(check-type (hash-ref/inst a-hash "smelly")
            : Int
            ⇒ 0)

(check-type ((inst hash-count String Int) a-hash)
            : Int
            ⇒ 3)
