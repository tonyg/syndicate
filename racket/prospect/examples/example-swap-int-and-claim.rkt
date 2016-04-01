#lang prospect
;; Analogous to nc-incremental-swap-int-and-claim.rkt in the Redex model.
;; Demonstrates (hopefully) correct processing of feedback at interest switches.

(require rackunit)

(spawn (lambda (e u)
         (if (< (length u) 3)
             (transition
              (append u (list e))
              (if (null? u)
                  (patch-seq (assert 'a) (unsub 'a))
                  '()))
             (begin0 #f (check-equal? u (list #f #f #f)))))
       '()
       (sub 'a))

(spawn (lambda (e u)
         (if (< (length u) 3)
             (transition
              (append u (list e))
              (if (null? u)
                  (patch-seq (retract 'b) (sub 'b))
                  '()))
             (begin0 #f (check-equal? u (list #f #f #f)))))
       '()
       (assert 'b))
