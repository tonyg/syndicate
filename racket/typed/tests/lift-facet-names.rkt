#lang typed/syndicate

(define (start)
  (start-facet room
    #f))

(define (another)
  (start-facet
    (field [room #f])
    (assert (tuple (! room)))))

(module+ test
  (define-type-alias ROOM room))
