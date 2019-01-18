#lang syndicate

(require/activate syndicate/drivers/tcp2)
(require (only-in racket/port read-bytes-line-evt))

(spawn (define id 'chat)
       (assert (tcp-connection id (tcp-address "localhost" 5999)))
       (stop-when (retracted (tcp-accepted id)))
       (on (message (tcp-in id $bs))
           (write-bytes bs)
           (flush-output))

       (define stdin-evt (read-bytes-line-evt (current-input-port) 'any))
       (stop-when (message (inbound (external-event stdin-evt (list (? eof-object? _))))))
       (on (message (inbound (external-event stdin-evt (list (? bytes? $line)))))
           (send! (tcp-out id line))
           ;; chat-tcp2 uses the line-reader, so need line separators.
           (send! (tcp-out id #"\n"))))
