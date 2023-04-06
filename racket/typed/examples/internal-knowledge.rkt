#lang typed/syndicate

;; Expected Output:
#|
balance = 0
balance = 5
balance = 0
JEEPERS
know overdraft!
balance = -1
balance = -2
no longer in overdraft
balance = 8
|#

(assertion-struct balance : Balance (v))
(message-struct deposit : Deposit (v))

;; Internal Events
(message-struct new-transaction : NewTransaction (old new))
(assertion-struct overdraft : Overdraft ())

(define-type-alias τc/external
  (U (Balance Int)
     (Message (Deposit Int))
     (Observe ★/t)))

(define-type-alias τc/internal
  (U (Message (NewTransaction Int Int))
     (Overdraft)
     (Observe ★/t)))

(define-type-alias τc
  (U τc/external
     τc/internal))

(run-ground-dataspace τc/external

(spawn
 (begin
 (start-facet bank
   (field [account Int 0])

   (assert (balance (ref account)))

   (on (message (deposit $v))
       (define prev (ref account))
       (set! account (+ v prev))
       (realize! (new-transaction prev (ref account))))

   (on (realize (new-transaction $old:Int $new:Int))
       (when (and (negative? new)
                  (not (negative? old)))
         (start-facet neg
          ;; (this print is to make sure only one of these facets is created)
          (printf "JEEPERS\n")
          (know (overdraft))
          (on (realize (new-transaction ★ $new:Int))
              (when (not (negative? new))
                (stop neg))))))

   (during (know (overdraft))
       (on-start (printf "know overdraft!\n"))
       (on-stop (printf "no longer in overdraft\n"))))))

(spawn
 (start-facet obs
   (on (asserted (balance $v))
       (printf "balance = ~a\n" v))))

(spawn
 (start-facet _
   (on start
    (send! (deposit 5))
    (spawn
     (start-facet _
      (on start
       (send! (deposit -5))
       (spawn
        (start-facet _
         (on start
          (send! (deposit -1))
          (spawn
           (start-facet _
            (on start
             (send! (deposit -1))
             (spawn (start-facet _ (on start (send! (deposit 10)))))))))))))))))
)
