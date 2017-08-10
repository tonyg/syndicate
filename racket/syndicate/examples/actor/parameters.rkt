#lang syndicate
;; Demonstrate use of parameters with actors.
;;
;; Per https://github.com/tonyg/syndicate/issues/10, "Parameters don't
;; work well with actors". Output from this program running against
;; buggy Syndicate:
;;
;; spawn-ps: (set)
;; start-ps: (set)
;; ps: (set)
;; start-ps: (set 'unset)
;; spawn-ps: (set 'second 'unset 'first)
;; ps: (set 'unset)
;; Survey-response: 'unset
;; Survey-response: 'unset
;; Survey-response: 'unset
;;
;; ... and against a fixed Syndicate:
;;
;; spawn-ps: (set)
;; start-ps: (set)
;; ps: (set)
;; start-ps: (set 'second 'unset 'first)
;; spawn-ps: (set 'second 'unset 'first)
;; ps: (set 'second 'unset 'first)
;; Survey-response: 'unset
;; Survey-response: 'first
;; Survey-response: 'second

(define p (make-parameter 'unset))

(define (spawn-one)
  (define p-at-spawn-time (p))
  (spawn #:name (list 'spawn-one p-at-spawn-time)
         (define p-at-start-time (p))
         (assert `(p-at-spawn-time ,p-at-spawn-time))
         (assert `(p-at-start-time ,p-at-start-time))
         (assert `(p ,(p)))
         (on (message 'survey)
             (send! `(survey-response ,(p))))))

(spawn*
 (spawn-one)
 (parameterize ((p 'first)) (spawn-one))
 (parameterize ((p 'second)) (spawn-one))
 (flush!)
 (until (asserted (observe 'survey)))
 (react (on-start (send! 'survey))
        (define/query-set spawn-ps `(p-at-spawn-time ,$v) v)
        (define/query-set start-ps `(p-at-start-time ,$v) v)
        (define/query-set ps `(p ,$v) v)
        (begin/dataflow (printf "spawn-ps: ~v\n" (spawn-ps)))
        (begin/dataflow (printf "start-ps: ~v\n" (start-ps)))
        (begin/dataflow (printf "ps: ~v\n" (ps)))
        (on (message `(survey-response ,$v))
            (printf "Survey-response: ~v\n" v))))
