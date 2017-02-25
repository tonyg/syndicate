#lang syndicate
;; Check that nested-world assertions are properly retracted.
;; Should print two "Got SCN:" patches - one adding, and one removing (observe 'die).

#;(actor (lambda (e s)
         (match e
           [(message 'die) (quit)]
           [_ #f]))
       (void)
       (patch-seq (sub 'die)
                  (sub (observe 'die))))

(dataspace-actor
 (actor (lambda (e s)
          (match e
            [(message (inbound 'die)) (quit)]
            [_ #f]))
        (void)
        (patch-seq (sub (inbound 'die))
                   (sub (inbound (observe 'die))))))

(actor (lambda (e s)
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
