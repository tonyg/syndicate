#lang typed/syndicate

(require rackunit/turnstile)

;; make sure that we can look at the type of the facet without the ρ in it
(check-type
 (role-strings
   (start-facet x
     (define (push-results)
       (cond
         [(zero? 0)
          (start-facet done (assert #t))]
         [else
          #f]))
    (define (∀ (ρ) (perform-task [k : (proc -> ★/t #:roles (ρ))]))
        (start-facet perform
         (on start (stop perform (k)))))
    (on start (call/inst perform-task push-results))))
 : (List String)
 -> (list
     "(Role (x) (Reacts OnStart (Role (perform) (Reacts OnStart (Stop perform (Branch (Effs (Role (done) (Shares True))) (Effs)))))))"))
