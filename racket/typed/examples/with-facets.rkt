#lang typed/syndicate

(define (wf1)
(spawn
 (with-facets
   ([onn (facet (assert (tuple 'on))
                (on start (printf "on\n")))]
    [off (facet (on (asserted (tuple 'go))
                    (stop off
                          (start onn)))
                (on start (printf "off\n")))])
   off)))

(run-ground-dataspace
 (wf1)
 (spawn (start-facet _ (assert (tuple 'go)))))

;; BAD
#;(spawn
 (with-facets
   [on (facet (on start (start on)))]
   on))
