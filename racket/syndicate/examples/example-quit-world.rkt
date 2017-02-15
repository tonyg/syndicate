#lang syndicate
;; Demonstrates quit-dataspace.

(require (only-in racket/port read-bytes-line-evt))

(define (spawn-command-listener)
  (actor (lambda (e s)
           (match e
             [(message (inbound (inbound (external-event _ (list #"quit")))))
              (printf "Quitting just the leaf actor.\n")
              (quit)]
             [(message (inbound (inbound (external-event _ (list #"quit-dataspace")))))
              (printf "Terminating the whole dataspace.\n")
              (transition s (quit-dataspace))]
             [_ #f]))
         (void)
         (sub (inbound (inbound
                        (external-event (read-bytes-line-evt (current-input-port) 'any) ?))))))

(define (spawn-ticker)
  (define (sub-to-alarm)
    (sub (inbound (inbound
                   (external-event (alarm-evt (+ (current-inexact-milliseconds) 1000)) ?)))))
  (actor (lambda (e s)
           (match e
             [(message (inbound (inbound (external-event _ _))))
              (printf "Tick!\n")
              (transition s
                          (list (retract ?)
                                (sub-to-alarm)))]
             [_ #f]))
         (void)
         (sub-to-alarm)))

(printf "Type 'quit' or 'quit-dataspace'.\n")
(spawn-dataspace (spawn-command-listener)
                 (spawn-ticker))
