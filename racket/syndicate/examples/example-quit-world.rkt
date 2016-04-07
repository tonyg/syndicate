#lang syndicate
;; Demonstrates quit-dataspace.

(require (only-in racket/port read-bytes-line-evt))

(define (spawn-command-listener)
  (spawn (lambda (e s)
           (match e
             [(message (at-meta (at-meta (external-event _ (list #"quit")))))
              (printf "Quitting just the leaf actor.\n")
              (quit)]
             [(message (at-meta (at-meta (external-event _ (list #"quit-dataspace")))))
              (printf "Terminating the whole dataspace.\n")
              (transition s (quit-dataspace))]
             [_ #f]))
         (void)
         (sub (external-event (read-bytes-line-evt (current-input-port) 'any) ?)
              #:meta-level 2)))

(define (spawn-ticker)
  (define (sub-to-alarm)
    (sub (external-event (alarm-evt (+ (current-inexact-milliseconds) 1000)) ?) #:meta-level 2))
  (spawn (lambda (e s)
           (match e
             [(message (at-meta (at-meta (external-event _ _))))
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
