#lang prospect
;; After Figure 1 in "Logic and lattices for distributed programming",
;; Conway et. al, UCB tech report, 2012

(require racket/set)
(require prospect/actor)

(struct link (from to cost) #:prefab)
(struct path (from to seen cost) #:prefab)
(struct path-exists (from to) #:prefab) ;; Hmm.
(struct min-cost (from to cost) #:prefab)

(actor (forever (assert (link 1 3 -2))
                (assert (link 2 1 4))
                (assert (link 2 3 3))
                (assert (link 3 4 2))
                (assert (link 4 2 -1))))

(actor (forever (during (link $from $to $cost)
                        (assert (path-exists from to))
                        (assert (path from to (set from to) cost)))))

(actor (forever (during (link $A $B $link-cost)
                        (during (path B $C $seen $path-cost)
                                (assert #:when (not (set-member? seen A)) (path-exists A C))
                                (assert #:when (not (set-member? seen A))
                                        (path A C (set-add seen A) (+ link-cost path-cost)))))))

(actor (forever (during (path-exists $from $to)
                        #:collect [(costs (set)) (least +inf.0)]
                        (assert (min-cost from to least))
                        (on (asserted (path from to _ $cost))
                            (values (set-add costs cost)
                                    (min least cost)))
                        (on (retracted (path from to _ $cost))
                            (define new-costs (set-remove costs cost))
                            (values new-costs
                                    (for/fold [(least +inf.0)] [(x new-costs)] (min x least)))))))

(actor (forever (during (path $from $to $seen $cost)
                        #:init [(displayln `(+ ,(path from to seen cost)))]
                        #:done [(displayln `(- ,(path from to seen cost)))])))
(actor (forever (on (asserted (min-cost $from $to $cost))
                    (displayln (min-cost from to cost)))))
