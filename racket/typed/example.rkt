#lang typed/syndicate

#;(require racket/base)

;; really lame how many times I have to write the dataspace type

(print-type
 (λ [10 (begin)]
     [(bind x Int)
      (facet _ (fields)
             (assert (tuple "set thing" (+ x 1))))]))

#;(dataspace (U (Observe (Tuple String ★)) (Tuple String Int))

           (spawn (U (Observe (Tuple String ★)) (Tuple String Int))
                  (facet _
                         (fields [the-thing Int 0])
                         (assert (tuple "the thing" (ref the-thing)))
                         (on (asserted (tuple "set thing" (bind new-v Int)))
                             (set! the-thing new-v))))

           (spawn (U (Observe (Tuple String ★)) (Tuple String Int))
                  (facet _ (fields)
                         (on (asserted (tuple "the thing" (bind x Int)))
                             (facet _ (fields)
                                    (assert (tuple "set thing" (+ x 1)))))))

           (spawn (U (Observe (Tuple String ★)) (Tuple String Int))
                  (facet _ (fields)
                         (on (asserted (tuple "the thing" (bind t Int)))
                             (displayln t)))))