#lang syndicate
;; Demonstrates that facets with no endpoints don't outlive their `add-facet!` call.

;; This actor will have two facets briefly, before dropping to one:
(spawn (on-start (react (on-start (printf "Hi 1!\n"))
                        (on-stop (printf "Bye 1!\n"))))
       (assert 'x))

;; This actor will have one facet briefly, before dropping to zero and terminating:
(spawn (on-start (printf "Hi 2!\n"))
       (on-stop (printf "Bye 2!\n")))

;; This actor will spawn a couple of nested facets, and when the inner
;; one terminates, the outer one will also be terminated:
(spawn (on-start
        (printf "3 outer\n")
        (react (on-start (printf "3 inner\n")
                         (send! 'terminate-three))
               (on (message 'terminate-three)
                   (printf "triggering 3 inner stop\n")
                   (stop-current-facet))
               (on-stop (printf "3 inner stop\n"))))
       (on-stop
        (printf "3 outer stop\n")))
