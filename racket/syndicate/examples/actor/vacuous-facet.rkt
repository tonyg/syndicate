#lang syndicate/actor
;; Demonstrates that facets with no endpoints don't outlive their `add-facet!` call.

;; This actor will have two facets briefly, before dropping to one:
(spawn (on-start (react (on-start (printf "Hi 1!\n"))
                        (on-stop (printf "Bye 1!\n"))))
       (assert 'x))

;; This actor will have one facet briefly, before dropping to zero and terminating:
(spawn (on-start (printf "Hi 2!\n"))
       (on-stop (printf "Bye 2!\n")))
