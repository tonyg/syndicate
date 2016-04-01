#lang prospect

(require racket/set)
(require "../trie.rkt")
(require "../demand-matcher.rkt")
(require "../drivers/timer.rkt")

(spawn-timer-driver)

(spawn (lambda (e old-count)
         (match e
           [(? patch?)
            (define-values (in out) (patch-project/set #:take 2 e `(parent ,(?!) ,(?!))))
            (define new-count (+ old-count (set-count in) (- (set-count out))))
            (printf "New parent-record count: ~v\n" new-count)
            (transition new-count
                        (list (retract `(parent-count ,?))
                              (assert `(parent-count ,new-count))))]
           [_ #f]))
       0
       (patch-seq (sub `(parent ,? ,?))
                  (assert `(parent-count 0))))

(define (insert-record record . monitors)
  (printf "Record ~v inserted, depending on ~v\n" record monitors)
  (spawn (lambda (e s)
           (match e
             [(? patch/removed?)
              (printf "Retracting ~v because dependencies ~v vanished\n"
                      record
                      (set->list (trie-project/set/single (patch-removed e) (?!))))
              (quit)]
             [(message `(retract ,(== record)))
              (printf "Retracting ~v because we were told to explicitly\n" record)
              (quit)]
             [_ #f]))
         (void)
         (patch-seq (assert record)
                    (sub `(retract ,record))
                    (patch-seq* (map sub monitors)))))

(insert-record `(parent john douglas))
(insert-record `(parent bob john))
(insert-record `(parent ebbon bob))

(spawn (lambda (e s)
         (match e
           [(? patch?)
            (transition s
                        (for/list [(AB (trie-project/set #:take 2
                                                         (patch-added e)
                                                         `(parent ,(?!) ,(?!))))]
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
                        (for/list [(AC (trie-project/set #:take 2
                                                         (patch-added e)
                                                         `(parent ,(?!) ,(?!))))]
                          (match-define (list A C) AC)
                          (printf "Inductive step for ~v asserted\n" `(parent ,A ,C))
                          (spawn (lambda (e s)
                                   (define removed-parents
                                     (and (patch? e)
                                          (trie-project (patch-removed e) `(parent ,(?!) ,(?!)))))
                                   (if (trie-non-empty? removed-parents)
                                       (begin
                                         (printf
                                          "Inductive step for ~v retracted because of removal ~v\n"
                                          `(parent ,A ,C)
                                          (trie-key-set #:take 2 removed-parents))
                                         (quit))
                                       (and (patch? e)
                                            (transition s
                                                        (for/list [(CB (trie-project/set
                                                                        #:take 2
                                                                        (patch-added e)
                                                                        `(ancestor ,(?!) ,(?!))))]
                                                          (match-define (list _ B) CB)
                                                          (insert-record `(ancestor ,A ,B)
                                                                         `(parent ,A ,C)
                                                                         `(ancestor ,C ,B)))))))
                                 (void)
                                 (patch-seq (sub `(parent ,A ,C))
                                            (sub `(ancestor ,C ,?))))))]
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
;;                                     (define new-facts (trie-union old-facts (patch-added e)))
;;                                     (define triples (trie-project/set #:take 3 new-facts
;;                                                                       `(,(?!) ,(?!) ,(?!))))
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
;;                                trie-empty
;;                                (patch-seq
;;                                 (sub `(parent ,A ,B))
;;                                 (sub `(parent ,A ,?))
;;                                 (sub `(ancestor ,? ,B))
;;                                 (pub `(ancestor ,A ,B))))))

(spawn (lambda (e s)
         (when (patch? e) (pretty-print-patch e))
         #f)
       (void)
       (sub `(ancestor ebbon douglas)))

(define (after msec thunk)
  (define id (gensym 'after))
  (if (zero? msec)
      (thunk)
      (spawn (lambda (e s) (and (message? e) (quit (thunk))))
             (void)
             (list (message (set-timer id msec 'relative))
                   (sub (timer-expired id ?))))))

(define use-delays? #t)

(after (if use-delays? 1000 0) (lambda ()
                                 (printf "----- Retracting\n")
                                 (message `(retract (parent bob john)))))
(after (if use-delays? 2000 0) (lambda ()
                                 (printf "----- Asserting\n")
                                 (list (insert-record `(parent bob mary))
                                       (insert-record `(parent mary sue))
                                       (insert-record `(parent sue john)))))
