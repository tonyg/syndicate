#lang syndicate
;; Check that nested-world assertions are properly retracted.
;; Should print two "Got SCN:" patches - one adding, and one removing (observe 'die).

#;(spawn (lambda (e s)
         (match e
           [(message 'die) (quit)]
           [_ #f]))
       (void)
       (patch-seq (sub 'die)
                  (sub (observe 'die))))

(spawn-dataspace
 (spawn (lambda (e s)
          (match e
            [(message (at-meta 'die)) (quit)]
            [_ #f]))
        (void)
        (patch-seq (sub 'die #:meta-level 1)
                   (sub (observe 'die) #:meta-level 1))))

(spawn (lambda (e s)
         (match e
           [(? patch? p)
            (printf "Got SCN:\n")
            (pretty-print-patch p)
            (transition s (if (patch/added? p)
                              (message 'die)
                              '()))]
           [_ #f]))
       (void)
       (sub (observe 'die)))
