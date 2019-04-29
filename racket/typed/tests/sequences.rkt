#lang typed/syndicate/roles

(require rackunit/turnstile)

(check-type empty-sequence : (Sequence (U)))

(typecheck-fail (sequence-length empty-sequence))

(check-type ((inst sequence-length (U)) empty-sequence)
            : Int
            ⇒ 0)

(define sequence-length/Int (inst sequence-length Int))
(define sequence->list/Int (inst sequence->list Int))
(define in-list/Int (inst in-list Int))

(check-type (sequence->list/Int (in-list/Int (list 3 9 20)))
            : (List Int)
            ⇒ (list 3 9 20))
