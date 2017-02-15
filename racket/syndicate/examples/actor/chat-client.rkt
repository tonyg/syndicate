#lang syndicate/actor

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/port read-bytes-line-evt))

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))
(define stdin-evt (read-bytes-line-evt (current-input-port) 'any))

(spawn (stop-when (message (inbound (external-event stdin-evt (list (? eof-object? _))))))
       (stop-when (retracted (advertise (tcp-channel remote-handle local-handle _))))
       (assert (advertise (tcp-channel local-handle remote-handle _)))

       (on (message (inbound (external-event stdin-evt (list (? bytes? $line)))))
           (send! (tcp-channel local-handle remote-handle line)))

       (on (message (tcp-channel remote-handle local-handle $bs))
           (write-bytes bs)
           (flush-output)))
