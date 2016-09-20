#lang syndicate/actor

(require syndicate/firewall)

(struct m (b) #:prefab)
(struct a (v) #:prefab)

(actor (on (message (m $b))
           (printf "Message: ~v\n" b))
       (on (asserted (a $v))
           (printf "Asserted: ~v\n" v))
       (on (retracted (a $v))
           (printf "Retracted: ~v\n" v)))

(firewall [(allow (m 'ok1))
           (allow (m 'ok2))]
          (on-start (send! (m 'ok1))
                    (send! (m 'ok2))
                    (send! (m 'must-not-allow))))

(firewall [(allow (m ?))
           (forbid (m 'must-not-allow))]
          (on-start (send! (m 'ok1))
                    (send! (m 'ok2))
                    (send! (m 'must-not-allow))))

(firewall [(allow (a 'ok1))
           (allow (a (list ?)))
           (forbid (a (list 'forbidden)))
           (allow (a 'ok2))]
          (assert (a 'ok1))
          (assert (a 'ok2))
          (assert (a (list 'ok3)))
          (assert (a (list 'forbidden))))

(firewall [(allow (a 'ok-wild1))
           (allow (a 'ok-wild2))]
          (assert (a _)))

(firewall [(allow (a 'ok-kid))]
          (assert (a 'forbidden-parent))
          (on-start (actor (assert (a _)))))
