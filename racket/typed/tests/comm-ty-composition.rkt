#lang typed/syndicate

(assertion-struct ping : Ping (v))
(assertion-struct pong : Pong (v))

(assertion-struct flip : Flip (v))
(assertion-struct flop : Flop (v))

(define-type-alias Pinger (Ping Int))
(define-type-alias Ponger (U (Ping Int)
                             (Pong Int)
                             (Observe (Ping ★/t))))
(define-type-alias PingPong (U Pinger Ponger))

(define-type-alias Flipper (Flip Int))
(define-type-alias Flopper (U (Flip Int)
                              (Flop Int)
                              (Observe (Flip ★/t))))
(define-type-alias FlipFlop (U Flipper Flopper))

(run-ground-dataspace (U PingPong FlipFlop)
  (spawn Pinger (start-facet _ (assert (ping 5))))
  (spawn Ponger (start-facet _ (during (ping $v) (assert (pong v)))))

  (spawn Flipper (start-facet _ (assert (flip 8))))
  (spawn Flopper (start-facet _ (during (flip $v) (assert (flop v))))))
