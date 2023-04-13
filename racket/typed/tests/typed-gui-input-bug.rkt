#lang typed/syndicate

(define-constructor* (a [v : Int]))
(define-constructor* (b [v : Int]))
(define-type-alias AB (U A B))
(define-constructor* (m [v : AB]))

(define (s)
  (spawn
    (react
      (during/spawn (m _)
        (stop-when (asserted (observe (m (a ★)))))
        (on (asserted (m (a $v)))
            #f)))))

(define (t)
  (spawn
    (react
      (on-start
       (spawn
         (react
           (on (message (m _))
               #f))))
      (on (message (m $ab))
          #f))))
