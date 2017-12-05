#lang syndicate
;; Sketch of a DNS intra-program protocol

(assertion-struct dns-entry (name address))

(require/activate syndicate/drivers/timestate)

(spawn #:name 'server
       (during (observe (dns-entry "localhost" _))
         (on-start (printf "asserting localhost record\n"))
         (on-stop (printf "retracting localhost record\n"))
         (assert (dns-entry "localhost" "127.0.0.1"))))

(spawn #:name 'cache
       (on (asserted (dns-entry $name $addr))
           (define deadline (+ (current-inexact-milliseconds) 5000))
           (react (stop-when (asserted (later-than deadline)))
                  (on-start (printf "caching ~a = ~a\n" name addr))
                  (on-stop (printf "uncaching ~a = ~a\n" name addr))
                  (assert (dns-entry name addr)))))

(spawn #:name 'main
       (stop-when (asserted (dns-entry "localhost" $addr))
         (printf "localhost is ~a\n" addr)
         (sleep 1)
         (react (stop-when (asserted (dns-entry "localhost" $addr))
                  (printf "localhost is still ~a\n" addr)))))
