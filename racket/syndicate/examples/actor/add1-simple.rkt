#lang syndicate
;; Trivial example program to demonstrate tracing

(assertion-struct one-plus (n m))

(spawn #:name 'add1-server
       (during/spawn (observe (one-plus $n _))
         #:name (list 'solving 'one-plus n)
         (assert (one-plus n (+ n 1)))))

(spawn #:name 'client-process
       (stop-when (asserted (one-plus 3 $value))
         (printf "1 + 3 = ~a\n" value)))
