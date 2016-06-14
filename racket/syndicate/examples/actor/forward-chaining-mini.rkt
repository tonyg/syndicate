#lang syndicate
;; Minimal syndicate/actor variation on examples/forward-chaining.rkt.

(require syndicate/actor)

(actor (forever (assert `(parent john douglas))))
(actor (forever (assert `(parent bob john))))
(actor (forever (assert `(parent ebbon bob))))

;; This looks like an implication:
;; (parent A C) ⇒ ((ancestor A C) ∧ ((ancestor C B) ⇒ (ancestor A B)))
;;
(actor (forever (during `(parent ,$A ,$C)
                        (assert `(ancestor ,A ,C))
                        (during `(ancestor ,C ,$B)
                                (assert `(ancestor ,A ,B))))))

(actor (forever (on (asserted `(ancestor ,$A ,$B))
                    (log-info "~a is an ancestor of ~a" A B))))
