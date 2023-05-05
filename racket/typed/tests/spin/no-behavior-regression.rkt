#lang typed/syndicate

;; Previously, this would generate an empty atomic block in the SPIN program for
;; the initial actions, which is illegal syntax apparently

(define (spawn-A)
  (spawn (start-facet root #f)))

(module+ test
  (verify-actors TT
    (spawn-A)))
