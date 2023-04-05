#lang typed/syndicate

;; The point of this test is to show that the SPIN output captures the fact that
;; A's event handler repeatedly fires, even though it doesn't explicitly pay
;; attention to the retraction of (tuple) assertions.

(define (spawn-A)
(spawn
  (start-facet AA
      (on (asserted (tuple))
          (displayln 'got)
          (send! (tuple 1))))))

(define (spawn-B)
  (spawn
    (start-facet B
      (define (once)
        (start-facet xx
          (assert (tuple))
          (on (message (tuple 1))
              (stop xx)
              (displayln 'send)
              (send! (tuple "again")))))
      (on start (once))
      (on (message (tuple "again"))
          (once)))))

(module+ main
  (run-ground-dataspace
   (spawn-A)
   (spawn-B)
   (spawn
     (start-facet
         (on (retracted (tuple))
             (displayln 'gone))))))

(module+ test
  (verify-actors (Always (Eventually (M (Tuple NonZero))))
    AA
    B))
