#lang syndicate/actor
;; Demonstrate handling of facet termination.
;;
;; Prior to early November, 2016, only a *single* stop-when was able
;; to respond to a given termination event. Any others that happened
;; to match would not fire; any `on` clauses may or may not fire,
;; nondeterministically.
;;
;; Since then, I've altered the termination protocol to honour one of
;; the core Syndicate design ideas: that a single event goes to *all*
;; interested parties. The upshot of this, in this context, is that
;; given an event E, *all* `on E` and `stop-when E` should fire.
;;
;; See emails sent around the 6th October, and uni.org entry of 7
;; November.

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

 (on-start
  (react
   (assert (presence 'inner))
   (on-start (milestone! 'on-start 'inner))
   (on-stop (milestone! 'on-stop 'inner))

   (on (message 'E)
       (milestone! 'on-E 'inner 'pre-stop)
       (stop-facet (current-facet-id)
                   (milestone! 'on-E 'inner 'post-stop))
       (milestone! 'on-E 'inner 'mid-stop))

   (stop-when (message 'E)
              (milestone! 'stop-when 'inner 'pre-innermost)
              (react (assert (presence 'innermost))
                     (on-start (milestone! 'on-start 'innermost))
                     (on-stop (milestone! 'on-stop 'innermost))
                     (on (rising-edge #t) (milestone! 'on-rising-edge 'innermost))
                     (stop-when (rising-edge #t) (milestone! 'stop-when-rising-edge 'innermost))
                     (stop-when (message 'E)
                                (milestone! 'stop-when 'innermost 'SHOULD-NEVER-HAPPEN)))
              (milestone! 'stop-when 'inner 'post-innermost))))

 (on (message 'E)
     (milestone! 'on-E 'outer 'pre-stop)
     (stop-facet (current-facet-id)
                 (milestone! 'on-E 'outer 'post-stop))
     (milestone! 'on-E 'outer 'mid-stop)))
