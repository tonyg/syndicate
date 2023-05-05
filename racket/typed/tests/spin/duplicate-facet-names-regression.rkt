#lang typed/syndicate

;; Previously, this would generate two `mtype = {roo}` declarations, which is illegal in SPIN

(define (spawn-A)
  (spawn (start-facet root #f)))

(define (spawn-B)
  (spawn (start-facet root (on-start (stop root)))))

(module+ test
  (verify-actors TT
    (spawn-A)
    (spawn-A))

  (verify-actors TT
    (spawn-B)
    (spawn-B)))
