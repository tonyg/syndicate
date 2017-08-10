#lang syndicate
;; Demonstrate handling of facet termination.

(require racket/pretty)

(struct milestone (facet-id message) #:prefab)
(struct presence (detail) #:prefab)

(define (milestone! . detail)
  (printf ">>> ~v ~v\n" (current-facet-id) detail)
  (send! (milestone (current-facet-id) detail)))

(spawn (field [trace-rev '()])
       (define (push! w x) (trace-rev (cons (list w x) (trace-rev))))
       (on-start
        (until (asserted (observe 'E)))
        (send! 'E)
        (until (retracted (observe 'E)))
        (flush!)
        (flush!)
        (flush!)
        (pretty-print (reverse (trace-rev))))

       (on (asserted (presence $p)) (push! '+ (presence p)))
       (on (retracted (presence $p)) (push! '- (presence p)))
       (on (message (milestone $w $d)) (push! '! (milestone w d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn
 (assert (presence 'outer))
 (on-start (milestone! 'on-start 'outer))
 (on-stop (milestone! 'on-stop 'outer))

 (on (message 'E)
     (milestone! 'on-E 'outer 'pre-stop)
     (stop-facet (current-facet-id)
                 (milestone! 'on-E 'outer 'post-stop))
     (milestone! 'on-E 'outer 'mid-stop)))
