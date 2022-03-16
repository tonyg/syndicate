#lang typed/syndicate
;; Expected Output:
;; got: new

(define-constructor* (something : SomethingT new blue)
  #:with Something (SomethingT String Int))

(define-type-alias τc
  (U Something
     (Observe★ SomethingT)))

(run-ground-dataspace
 τc
 (spawn
  (start-facet _
    (assert (something "new" 42))))
 (spawn
  (start-facet _
    (on (asserted (something $x 42))
        (printf "got: ~a\n" x))))
 )
