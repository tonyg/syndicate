#lang syndicate/actor
;; Example showing an interesting design flaw (?) (bug?) in the
;; old-school LLL demand-matcher when demand changes too quickly. The
;; port 6000 server is started, but by the time it starts monitoring
;; demand for its services, the demand is already gone, replaced with
;; demand for port 5999. This causes connections to be accepted on
;; port 6000 going nowhere.
;;
;; One "fix" is to use #:assertions to give the TCP listener actor
;; some initial interests, thus transferring responsibility
;; atomically. This has been implemented (in commit 2a0197b). However,
;; this doesn't completely eliminate all possible instances where
;; demand may change too quickly. See example-demand-matcher-glitch-bug2.rkt.
;;
;; A speculative idea is to use a kind of contract-monitor to enforce
;; the invariant that demand *cannot* fluctuate too rapidly. One might
;; write that "if (listen 6000) is asserted, then if (listen 6000) is
;; retracted, (observe (listen 6000)) must have been asserted in the
;; causal history of the retraction", but what does "causal history"
;; mean, precisely? And how can it be soundly and efficiently tracked?
;;
;; The only "fix" that both solves the problem and is currently
;; implementable that I have thought of is to modify the
;; demand-matcher to do something like `during/spawn` is doing, using
;; an auxiliary protocol to centralise tracking of demand and supply
;; at the demand-matcher rather than delegating it to the services.

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)

(struct listen-port (number) #:prefab) ;; assertion

(define default-port 6000)

(spawn #:name 'connection-acceptor
       (define/query-value port default-port (listen-port $v) v)
       (assert (advertise (observe (tcp-channel _ (tcp-listener (port)) _))))
       (during (advertise (tcp-channel $c (tcp-listener (port)) _))
         (let ((accepted-port (port))) ;; capture field value at time of connect
           (assert (advertise (tcp-channel (tcp-listener accepted-port) c _)))
           (on-start
            (printf "Accepted connection from ~v on port ~a\n" c accepted-port)
            (send! (tcp-channel (tcp-listener accepted-port) c #"Hello!\n")))
           (on-stop
            (printf "Closed connection ~v on port ~a\n" c accepted-port)))))

(spawn #:name 'configuration-provider
       (assert (listen-port 5999)))
