#lang typed/syndicate

(define (booler)
  (spawn
    (assert (tuple (ann #t Bool)))))

(define (onT)
  (spawn
    (on (asserted (tuple #t))
        (react
          (assert (tuple "hello"))))))

(define (onF)
  (spawn
    (on (asserted (tuple #f))
        (react
          (assert (tuple 'hello))))))

(module+ test
  (verify-actors/fail (Always (Not (Tuple Symbol)))
    (booler)
    (onT)
    (onF))

  (verify-actors/fail (Always (Not (Tuple String)))
    (booler)
    (onT)
    (onF))

  (verify-actors (Eventually (Or (Tuple String)
                                 (Tuple Symbol)))
    (booler)
    (onT)
    (onF))

  (verify-actors (Eventually (Or (And (Tuple String) (Not (Tuple Symbol)))
                                 (And (Tuple Symbol) (Not (Tuple String)))))
    (booler)
    (onT)
    (onF)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Same example, but with messages

(define (boolerM)
  (spawn
    (on (asserted (observe (tuple ★)))
        (send! (tuple (ann #t Bool))))))

(define (onTM)
  (spawn
    (on (message (tuple #t))
        (react
          (assert (tuple "hello"))))))

(define (onFM)
  (spawn
    (on (message (tuple #f))
        (react
          (assert (tuple 'hello))))))

(module+ test
  (verify-actors/fail (Always (Not (Tuple String)))
    (boolerM)
    (onTM)
    (onFM))

  (verify-actors/fail (Always (Not (Tuple Symbol)))
    (boolerM)
    (onTM)
    (onFM))

  (verify-actors (Eventually (Or (Tuple String)
                                   (Tuple Symbol)))
      (boolerM)
      (onTM)
      (onFM))

  (verify-actors (Eventually (Or (And (Tuple String) (Not (Tuple Symbol)))
                                   (And (Tuple Symbol) (Not (Tuple String)))))
      (boolerM)
      (onTM)
      (onFM)))
