#lang typed/syndicate

(run-ground-dataspace Int
  (spawn #:type Int
    (start-facet _
      (assert 42))))
