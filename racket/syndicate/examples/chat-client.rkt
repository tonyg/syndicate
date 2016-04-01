#lang syndicate

(require (only-in racket/port read-bytes-line-evt))
(require "../drivers/tcp.rkt")

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))

(spawn-tcp-driver)
(spawn/stateless (lambda (e)
                   (match e
                     [(? patch/removed?) (quit)]
                     [(message (at-meta (external-event _ (list (? eof-object?)))))
                      (quit)]
                     [(message (at-meta (external-event _ (list line))))
                      (message (tcp-channel local-handle remote-handle line))]
                     [(message (tcp-channel _ _ bs))
                      (write-bytes bs)
                      (flush-output)
                      #f]
                     [_ #f]))
                 (patch-seq
                  (sub (external-event (read-bytes-line-evt (current-input-port) 'any) ?)
                       #:meta-level 1)
                  (sub (tcp-channel remote-handle local-handle ?))
                  (sub (advertise (tcp-channel remote-handle local-handle ?)))
                  (pub (tcp-channel local-handle remote-handle ?))))
