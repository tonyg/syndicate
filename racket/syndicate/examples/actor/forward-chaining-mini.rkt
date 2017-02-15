#lang syndicate/actor
;; Minimal syndicate/actor variation on examples/forward-chaining.rkt.

(spawn (assert `(parent john douglas)))
(spawn (assert `(parent bob john)))
(spawn (assert `(parent ebbon bob)))

;; This looks like an implication:
;; (parent A C) ⇒ ((ancestor A C) ∧ ((ancestor C B) ⇒ (ancestor A B)))
;;
(spawn (during `(parent ,$A ,$C)
               (assert `(ancestor ,A ,C))
               (during `(ancestor ,C ,$B)
                       (assert `(ancestor ,A ,B)))))

(spawn (on (asserted `(ancestor ,$A ,$B))
           (log-info "~a is an ancestor of ~a" A B)))
