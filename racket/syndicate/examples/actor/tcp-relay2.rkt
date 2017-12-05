#lang syndicate
;; Simple TCP relay

(require/activate syndicate/drivers/tcp2)

(spawn (during (tcp-connection $id (tcp-listener 5000))
         (assert (tcp-accepted id))

         (define root-facet-id (current-facet-id))
         (define outbound-id (gensym 'outbound-id))

         (assert (tcp-connection outbound-id (tcp-address "localhost" 5999)))
         (during (tcp-accepted outbound-id)
           (on-stop (stop-facet root-facet-id))
           (on (message (tcp-in id $bs))
               (send! (tcp-out outbound-id bs)))
           (on (message (tcp-in outbound-id $bs))
               (send! (tcp-out id bs))))))
