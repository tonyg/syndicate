#lang typed/syndicate

(define-type-alias ds-type
   (U (Observe (Tuple String ★)) (Tuple String Int)))

(dataspace ds-type

           (spawn ds-type
                  (facet _
                         (fields [the-thing Int 0])
                         (assert (tuple "the thing" (ref the-thing)))
                         (on (asserted (tuple "set thing" (bind new-v Int)))
                             (set! the-thing new-v))))

           (spawn ds-type
                  (let [k (λ [10 (begin)]
                            [(bind x Int)
                             (facet _ (fields)
                                    (assert (tuple "set thing" (+ x 1))))])]
                    (facet _ (fields)
                           (on (asserted (tuple "the thing" (bind x Int)))
                               (k x)))))

           (spawn ds-type
                  (facet _ (fields)
                         (on (asserted (tuple "the thing" (bind t Int)))
                             (displayln t)))))