#lang typed/syndicate

(require rackunit/turnstile)

(define-constructor* (run [distance : Int] [windy? : Bool]))

(check-type
 (spawn
  (start-facet runner
    (assert (run 5 #t))))
 : ★/t)

(check-type
 (spawn
  (start-facet longer
    (on (asserted (run $d $w?))
        (printf "run ~a ~a\n" (add1 d) (if w? "brr" "")))))
 : ★/t)

(check-type
 (dataspace
  (spawn
   (start-facet runner
                (assert (run 5 #t))))
  (spawn
   (start-facet longer
                (on (asserted (run $d $w?))
                    (printf "run ~a ~a\n" (add1 d) (if w? "brr" ""))))))
 : ★/t)

(typecheck-fail
 (dataspace
  (spawn
   (start-facet runner
                ;; NB
                (assert (run "FAR" #t))))
  (spawn
   (start-facet longer
                (on (asserted (run $d $w?))
                    (printf "run ~a ~a\n" (add1 d) (if w? "brr" ""))))))
 #:verb-msg "unprepared to handle inputs: (RunT String Bool)")
