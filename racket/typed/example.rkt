#lang typed/syndicate

(require turnstile/rackunit-typechecking)

#;(spawn
 (assert (list "hello")))

#;(spawn
 (on (asserted (list "hello"))
     (printf "hello\n")))

(spawn ⊥ (assert "hello"))