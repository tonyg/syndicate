#lang typed/syndicate

(run-ground-dataspace Int
  (spawn Int
    (start-facet _
      (assert 42))))
