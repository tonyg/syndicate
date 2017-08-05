#lang syndicate/actor
;; Demonstrates glitch preservation in during/spawn.
;;
;; Previously, `spawn` was expanded in place into the new actor's
;; initial actions. This reordering was confusing, as demonstrated
;; here: the reordering of `spawn` ahead of the retraction of previous
;; supply caused uninterrupted supply, even though demand had glitched
;; and the supply instance had been replaced.
;;
;; Buggy output:
;;
;; Asserting demand.
;; Supply asserted.
;; Glitching demand.
;; Demand now steady.
;;
;; Correct output:
;;
;; Asserting demand.
;; Supply asserted.
;; Glitching demand.
;; Demand now steady.
;; Supply retracted.
;; Supply asserted.

(spawn (during/spawn 'demand
         (assert 'intermediate-demand)))

(spawn (during/spawn 'intermediate-demand
         (assert 'supply)))

(spawn* (react (on (asserted 'supply) (printf "Supply asserted.\n"))
               (on (retracted 'supply) (printf "Supply retracted.\n")))
        (until (asserted (observe 'demand)))
        (printf "Asserting demand.\n")
        (assert! 'demand)
        (until (asserted 'supply))
        (printf "Glitching demand.\n")
        (retract! 'demand)
        (flush!)
        (assert! 'demand)
        (printf "Demand now steady.\n"))
