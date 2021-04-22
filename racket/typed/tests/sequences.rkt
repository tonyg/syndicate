#lang typed/syndicate

(require rackunit/turnstile)

(check-type empty-sequence : (Sequence (U)))

(check-type (sequence-length empty-sequence)
            : Int
            ⇒ 0)

(define sequence-length/Int (inst sequence-length Int))
(define sequence->list/Int (inst sequence->list Int))
(define in-list/Int (inst in-list Int))

(check-type (sequence->list/Int (in-list/Int (list 3 9 20)))
            : (List Int)
            ⇒ (list 3 9 20))
