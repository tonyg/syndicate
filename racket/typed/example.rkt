#lang typed/syndicate

#;(require racket/base)

(spawn ⊥
       (facet _
              (assert "hello")))

(spawn ⊥
       (facet _
              (on (asserted "hello")
                  (unsafe-do (printf "got hello\n")))))