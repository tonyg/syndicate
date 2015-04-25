#lang prospect

(require racket/set)
(require "../route.rkt")
(require "../demand-matcher.rkt")
(require "../drivers/timer.rkt")

(spawn-timer-driver)

(spawn (lambda (e old-count)
         (match e
           [(? patch?)
            (define-values (in out)
              (patch-project/set e (compile-projection `(parent ,(?!) ,(?!)))))
            (define new-count (+ old-count (set-count in) (- (set-count out))))
            (printf "New parent-record count: ~v\n" new-count)
            (transition new-count
                        (list (retract `(parent-count ,?))
                              (assert `(parent-count ,new-count))))]
           [_ #f]))
       0
       (sub `(parent ,? ,?))
       (assert `(parent-count 0)))

(define (insert-record record . monitors)
  (printf "Record ~v inserted, depending on ~v\n" record monitors)
  (spawn (lambda (e s)
           (match e
             [(? patch/removed?)
              (printf "Retracting ~v because dependencies ~v vanished\n"
                      record
                      (set->list (matcher-project/set (patch-removed e) (compile-projection (?!)))))
              (quit)]
             [(message `(retract ,(== record)))
              (printf "Retracting ~v because we were told to explicitly\n" record)
              (quit)]
             [_ #f]))
         (void)
         (assert record)
         (sub `(retract ,record))
         (patch-seq* (map sub monitors))))

(insert-record `(parent john douglas))
(insert-record `(parent bob john))
(insert-record `(parent ebbon bob))

(spawn (lambda (e s)
         (match e
           [(? patch?)
            (transition s
                        (for/list [(AB (matcher-project/set
                                        (patch-added e)
                                        (compile-projection `(parent ,(?!) ,(?!)))))]
                          (match-define (list A B) AB)
                          (insert-record `(ancestor ,A ,B)
                                         `(parent ,A ,B))))]
           [_ #f]))
       (void)
       (sub `(parent ,? ,?)))

(spawn (lambda (e s)
         (match e
           [(? patch?)
            (transition s
                        (for/list [(AC (matcher-project/set
                                        (patch-added e)
                                        (compile-projection `(parent ,(?!) ,(?!)))))]
                          (match-define (list A C) AC)
                          (printf "Inductive step for ~v asserted\n" `(parent ,A ,C))
                          (spawn (lambda (e s)
                                   (define removed-parents
                                     (and (patch? e)
                                          (matcher-project (patch-removed e)
                                                           (compile-projection
                                                            `(parent ,(?!) ,(?!))))))
                                   (if (matcher-non-empty? removed-parents)
                                       (begin
                                         (printf
                                          "Inductive step for ~v retracted because of removal ~v\n"
                                          `(parent ,A ,C)
                                          (matcher-key-set removed-parents))
                                         (quit))
                                       (and (patch? e)
                                            (transition s
                                                        (for/list [(CB (matcher-project/set
                                                                        (patch-added e)
                                                                        (compile-projection
                                                                         `(ancestor ,(?!) ,(?!)))))]
                                                          (match-define (list _ B) CB)
                                                          (insert-record `(ancestor ,A ,B)
                                                                         `(parent ,A ,C)
                                                                         `(ancestor ,C ,B)))))))
                                 (void)
                                 (sub `(parent ,A ,C))
                                 (sub `(ancestor ,C ,?)))))]
           [_ #f]))
       (void)
       (sub `(parent ,? ,?)))

;;;; Backward-chaining, below, doesn't quite work as formulated with
;;;; this code snippet: the demand-matcher gets confused by wildcard
;;;; demand from the recursive step. One way out of this might be to
;;;; avoid patterns quantifying over more than one place at once: to
;;;; have a process for *all* potential ancestor-queries for a given
;;;; person, rather than one for each *specific* ancestor-query.
;;
;; (spawn-demand-matcher (observe `(ancestor ,(?!) ,(?!)))
;;                       (advertise `(ancestor ,(?!) ,(?!)))
;;                       (lambda (A B)
;;                         (spawn (lambda (e old-facts)
;;                                  (match e
;;                                    [(? patch/removed?) (quit)]
;;                                    [(? patch?)
;;                                     (define new-facts (matcher-union old-facts (patch-added e)))
;;                                     (define triples (matcher-project/set new-facts
;;                                                                          (compile-projection
;;                                                                           `(,(?!) ,(?!) ,(?!)))))
;;                                     (printf "Learned new facts: ~v\n" triples)
;;                                     (transition new-facts
;;                                                 (when (or (set-member? triples `(parent ,A ,B))
;;                                                           (for/or ((triple triples))
;;                                                             (match triple
;;                                                               [`(ancestor ,C ,(== B))
;;                                                                (set-member? triples `(parent ,A ,C))]
;;                                                               [_ #f])))
;;                                                   (printf "... and as a result, asserting ~v\n"
;;                                                           `(ancestor ,A ,B))
;;                                                   (assert `(ancestor ,A ,B))))]
;;                                    [_ #f]))
;;                                (matcher-empty)
;;                                (sub `(parent ,A ,B))
;;                                (sub `(parent ,A ,?))
;;                                (sub `(ancestor ,? ,B))
;;                                (pub `(ancestor ,A ,B)))))

(spawn (lambda (e s)
         (when (patch? e) (pretty-print-patch e))
         #f)
       (void)
       (sub `(ancestor ebbon douglas)))

(define (after msec thunk)
  (define id (gensym 'after))
  (if (zero? msec)
      (thunk)
      (list
       (spawn (lambda (e s) (and (message? e) (quit (thunk))))
              (void)
              (sub (timer-expired id ?)))
       (message (set-timer id msec 'relative)))))

(define use-delays? #t)

(after (if use-delays? 1000 0) (lambda ()
                                 (printf "----- Retracting\n")
                                 (message `(retract (parent bob john)))))
(after (if use-delays? 2000 0) (lambda ()
                                 (printf "----- Asserting\n")
                                 (list (insert-record `(parent bob mary))
                                       (insert-record `(parent mary sue))
                                       (insert-record `(parent sue john)))))
