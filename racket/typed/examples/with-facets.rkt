#lang typed/syndicate

(lambda ()
(spawn
 (with-facets
   ([onn (facet (assert (tuple 'on)))]
    [off (facet (on (asserted (tuple 'go))
                    (stop off
                          (start onn))))])
   off)))

;; BAD
#;(spawn
 (with-facets
   [on (facet (on start (start on)))]
   on))
