#lang typed/syndicate

(run-ground-dataspace (U)
(spawn #:type (U)
       (start-facet x
                    (field [y Int 0])
                    (define/dataflow x (add1 (ref y)))
                    (displayln (add1 (ref x)))
                    ;; print 2
                    #f))
)
