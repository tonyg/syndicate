#lang typed/syndicate/roles

;; Expected Output
;; pong: 8339

(message-struct ping : Ping (v))
(message-struct pong : Pong (v))

(define-type-alias ds-type
  (U (Message (Ping Int))
     (Message (Pong Int))
     (Observe (Ping ★/t))
     (Observe (Pong ★/t))
     (Observe (Observe (Ping ★/t)))))

(run-ground-dataspace ds-type
  (spawn ds-type
    (lift+define-role ponger
    (start-facet echo
      (on (message (ping $v))
          (send! (pong v))))))
  (spawn ds-type
    (lift+define-role pinger
    (start-facet serve
      (on (asserted (observe (ping _)))
          (send! (ping 8339)))
      (on (message (pong $x))
          (printf "pong: ~v\n" x))))))

(module+ test
  (verify-actors (And (Eventually (M (Ping Int)))
                      (Eventually (M (Pong Int)))
                      (Always (Implies (M (Ping Int))
                                       (Eventually (M (Pong Int))))))
                 pinger
                 ponger))
