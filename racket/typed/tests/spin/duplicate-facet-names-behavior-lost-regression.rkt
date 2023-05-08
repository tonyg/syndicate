#lang typed/syndicate

(define (spawn-A)
  (spawn
    (start-facet A
      (assert (tuple "hello"))
      (on-start
       (start-facet A
         (assert (tuple 'hello)))))))

(define (spawn-B)
  (spawn
    (during (tuple)
      (assert (tuple "hello"))
      (during (tuple "hello")
        (assert (tuple 'hello))))))

(module+ test
  (verify-actors (Eventually (And (Tuple String)
                                  (Tuple Symbol)))
    (spawn-A))

  (verify-actors (Eventually (And (Tuple String)
                                  (Tuple Symbol)))
    #:IO (Tuple)
    (spawn-B)))
