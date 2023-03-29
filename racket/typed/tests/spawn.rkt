#lang typed/syndicate

(require rackunit/turnstile)

(check-type
  (spawn (U (Tuple Int) (Observe (Tuple ★/t)))
   (start-facet _
     (on (asserted (tuple $x))
         (add1 x))))
  ;; wanted: ν-s ((Actor (Tuple Int)))
  : ★/t)

(typecheck-fail
 (spawn (U (Tuple String) (Observe (Tuple ★/t)))
        (start-facet _
                     (on (asserted (tuple $x:Int))
                         (add1 x))))
 #:with-msg "Not prepared to handle inputs:\n\\(Tuple String\\)")

(check-type
 (spawn (U)
        (start-facet _
                     (know (tuple 5))
                     (on (know (tuple $x:Int))
                         (add1 x))))
 ;; wanted: ν-s ((Actor (U)))
 : ★/t)

(typecheck-fail
 (spawn (U)
        (start-facet _
                     (know (tuple "hi"))
                     (on (know (tuple $x:Int))
                         (add1 x))))
 #:with-msg "Not prepared to handle inputs:\n\\(Tuple String\\)")
