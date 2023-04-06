#lang typed/syndicate

(define-constructor* (price [v : Int]))

(define (A)
  (spawn
    (react
      (during/spawn (price _)
        (on (asserted (price $v))
            #f)))))

(define (B)
  (spawn
    (react
      (during (observe (price ★))
        (assert (price (ann 5 Int)))))))

(define (C)
  (spawn
    (react
      (on (message (inbound (price _)))
          (react
            (on (message (inbound (price $v)))
                #f))))))

(run-ground-dataspace
 (A)
 (B))
