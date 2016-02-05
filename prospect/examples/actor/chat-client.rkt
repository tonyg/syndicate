#lang prospect

(require prospect/actor)
(require prospect/drivers/tcp)
(require (only-in racket/port read-bytes-line-evt))

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))
(define stdin-evt (read-bytes-line-evt (current-input-port) 'any))

(spawn-tcp-driver)

(forever (on (message (external-event stdin-evt (list $line)) #:meta-level 1)
             (if (eof-object? line)
                 (return!)
                 (send! (tcp-channel local-handle remote-handle line))))

         (assert (advertise (tcp-channel local-handle remote-handle _)))
         (on (retracted (advertise (tcp-channel remote-handle local-handle _))) (return!))
         (on (message (tcp-channel remote-handle local-handle $bs))
             (write-bytes bs)
             (flush-output)))
