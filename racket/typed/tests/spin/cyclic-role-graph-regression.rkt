#lang typed/syndicate

(define (spawn-post-factory)
  (spawn #:name 'post-factory
    (on (message ($ p0 (tuple $timestamp:Int)))
        (spawn #:name p0
          (field [items Int 0])
          ;; The problem was that this dataflow
          #;(define/dataflow p (tuple (! items)))
          ;; gave rise to an effect:
          ;; (Reacts OnDataflow (WritesField p) (ReadsField items))
          ;; and because Int is a union type, the below assertion becomes a var-assert, and the WritesField
          ;; effect becomes a Realize for the generated state machine, meaning the SPIN compiler can't ignore
          ;; that self-loop
          (assert (tuple (! items)))
          (on (message (tuple _:Int))
              (:= items 0))))))

(module+ test
  (verify-actors TT
    (spawn-post-factory)))

