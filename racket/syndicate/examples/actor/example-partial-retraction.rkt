#lang syndicate
;; Illustrates the response of asserted / retracted / during to
;; observation of assertions discarding some of their dimensions.

(require syndicate/actor)

(struct ready (what) #:prefab)
(struct entry (key val) #:prefab)

(actor (forever
        (assert (ready 'listener))
        (on (asserted (entry $key _))
            (log-info "key ~v asserted" key)
            (until (retracted (entry key _))
                   (on (asserted (entry key $value))
                       (log-info "add binding: ~v -> ~v" key value))
                   (on (retracted (entry key $value))
                       (log-info "del binding: ~v -> ~v" key value)))
            (log-info "key ~v retracted" key))))

(actor (forever
        (assert (ready 'other-listener))
        (during (entry $key _)
                #:init [(log-info "(other-listener) key ~v asserted" key)]
                #:done [(log-info "(other-listener) key ~v retracted" key)]
                (during (entry key $value)
                        #:init [(log-info "(other-listener) ~v ---> ~v" key value)]
                        #:done [(log-info "(other-listener) ~v -/-> ~v" key value)]))))

(define (pause)
  (log-info "pause")
  (until (asserted (ready 'pause))
         (assert (ready 'pause))))

(actor (until (asserted (ready 'listener)))
       (until (asserted (ready 'other-listener)))
       (assert! (entry 'a 1))
       (assert! (entry 'a 2))
       (assert! (entry 'b 3))
       (assert! (entry 'c 33))
       (assert! (entry 'a 4))
       (assert! (entry 'a 5))
       (pause)
       (retract! (entry 'a 2))
       (retract! (entry 'c 33))
       (assert! (entry 'a 9))
       (pause)
       (retract! (entry 'a ?))
       (pause))
