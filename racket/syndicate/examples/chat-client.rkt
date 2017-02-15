#lang syndicate

(require syndicate/protocol/advertise)
(require (only-in racket/port read-bytes-line-evt))
(require/activate "../drivers/tcp.rkt")

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))

(actor/stateless (lambda (e)
                   (match e
                     [(? patch/removed?) (quit)]
                     [(message (inbound (external-event _ (list (? eof-object?)))))
                      (quit)]
                     [(message (inbound (external-event _ (list line))))
                      (message (tcp-channel local-handle remote-handle line))]
                     [(message (tcp-channel _ _ bs))
                      (write-bytes bs)
                      (flush-output)
                      #f]
                     [_ #f]))
                 (patch-seq
                  (sub (inbound (external-event (read-bytes-line-evt (current-input-port) 'any) ?)))
                  (sub (tcp-channel remote-handle local-handle ?))
                  (sub (advertise (tcp-channel remote-handle local-handle ?)))
                  (pub (tcp-channel local-handle remote-handle ?))))
