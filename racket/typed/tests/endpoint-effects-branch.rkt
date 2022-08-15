#lang typed/syndicate

(lambda ()
  (start-facet x
    (if #f
        (assert (tuple 0))
        (assert (tuple 1)))))
