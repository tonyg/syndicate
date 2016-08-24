#lang syndicate/actor
;; Minimal syndicate/actor variation on examples/forward-chaining.rkt.

(actor (assert `(parent john douglas)))
(actor (assert `(parent bob john)))
(actor (assert `(parent ebbon bob)))

;; This looks like an implication:
;; (parent A C) ⇒ ((ancestor A C) ∧ ((ancestor C B) ⇒ (ancestor A B)))
;;
(actor (during `(parent ,$A ,$C)
               (assert `(ancestor ,A ,C))
               (during `(ancestor ,C ,$B)
                       (assert `(ancestor ,A ,B)))))

(actor (on (asserted `(ancestor ,$A ,$B))
           (log-info "~a is an ancestor of ~a" A B)))
