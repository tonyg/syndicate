#lang syndicate
;; Crude simplified TCP/IP driver interface. Should probably be fleshed out and made the primary
;; one, with tcp.rkt becoming deprecated and ultimately deleted.
;;
;; A nice refinement would be to introduce something like a `(tcp-error id _)` assertion, for when
;; something goes wrong listening or connecting. At present, for example, if connecting to some
;; other host that isn't listening, the tcp.rkt driver pretends the connection is open for an
;; infinitesimal instant before closing. This would be nicer if it never signalled "open" at all,
;; instead asserting something like `tcp-error` until interest in the connection goes away.

(provide (struct-out tcp-connection)
         (struct-out tcp-accepted)
         (struct-out tcp-out)
         (struct-out tcp-in)
         (struct-out tcp-in-line)
         (struct-out tcp-address)  ;; \_ From syndicate/drivers/tcp
         (struct-out tcp-listener) ;; /
         )

(require/activate syndicate/drivers/tcp)
(require syndicate/protocol/advertise)
(require syndicate/support/bytes)

(assertion-struct tcp-connection (id spec))
(assertion-struct tcp-accepted (id))
(message-struct tcp-out (id bytes))
(message-struct tcp-in (id bytes))
(message-struct tcp-in-line (id bytes))

(spawn #:name 'tcp2-listen-driver
       (during/spawn (observe (tcp-connection _ (tcp-listener $port)))
         #:name (list 'tcp2-listener port)
         (define us (tcp-listener port))
         (assert (advertise (observe (tcp-channel _ us _))))
         (on (asserted (advertise (tcp-channel $them us _)))
             (define id (seal (list them us)))
             (spawn #:name (list 'tcp2 'inbound id us)
                    (stop-when (retracted (advertise (tcp-channel them us _))))
                    (stop-when (retracted (tcp-accepted id)))
                    (assert (tcp-connection id us))
                    (on (message (tcp-channel them us $bs)) (send! (tcp-in id bs)))
                    (on (message (tcp-out id $bs)) (send! (tcp-channel us them bs)))))))

(spawn #:name 'tcp2-connect-driver
       (during/spawn (tcp-connection $id (tcp-address $host $port))
         #:name (list 'tcp2 'outbound (tcp-address host port) id)
         (define root-facet (current-facet-id))
         (define them (tcp-address host port))
         (define us (tcp-handle (seal id)))
         (during (advertise (tcp-channel them us _))
           (assert (tcp-accepted id))
           (on-stop (stop-facet root-facet)))
         (assert (advertise (tcp-channel us them _)))
         (on (message (tcp-channel them us $bs)) (send! (tcp-in id bs)))
         (on (message (tcp-out id $bs)) (send! (tcp-channel us them bs)))))

(spawn #:name 'tcp2-line-reader-factory
       (during/spawn (observe (tcp-in-line $id _))
         #:name (list 'tcp2-line-reader id)
         (field [buffer #""])
         (on (message (tcp-in id $bs)) (buffer (bytes-append (buffer) bs)))
         (begin/dataflow
           (define newline-pos (bytes-index (buffer) (char->integer #\newline)))
           (when newline-pos
             (define line (subbytes (buffer) 0 newline-pos))
             (buffer (subbytes (buffer) (+ newline-pos 1)))
             (send! (tcp-in-line id line))))))
