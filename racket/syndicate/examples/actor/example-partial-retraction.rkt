#lang syndicate/actor
;; Illustrates the response of asserted / retracted / during to
;; observation of assertions discarding some of their dimensions.

(struct ready (what) #:prefab)
(struct entry (key val) #:prefab)

(actor (react
        (assert (ready 'listener))
        (on (asserted (entry $key _))
            (log-info "key ~v asserted" key)
            (until (retracted (entry key _))
                   (on (asserted (entry key $value))
                       (log-info "add binding: ~v -> ~v" key value))
                   (on (retracted (entry key $value))
                       (log-info "del binding: ~v -> ~v" key value)))
            (log-info "key ~v retracted" key))))

(actor (react
        (assert (ready 'other-listener))
        (during (entry $key _)
                (log-info "(other-listener) key ~v asserted" key)
                (on-stop (log-info "(other-listener) key ~v retracted" key))
                (during (entry key $value)
                        (log-info "(other-listener) ~v ---> ~v" key value)
                        (on-stop (log-info "(other-listener) ~v -/-> ~v" key value))))))

(define (pause)
  (log-info "pause")
  (define token (gensym 'pause)) ;; FIXME:: If we use the same token every time, need epochs!
  (until (asserted (ready token))
         (assert (ready token))))

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
