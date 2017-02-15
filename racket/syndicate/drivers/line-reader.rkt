#lang syndicate/actor

(provide (struct-out tcp-channel-line))

(require "tcp.rkt")

(struct tcp-channel-line (source destination bytes) #:prefab)

;; This should probably be in the standard library.
(define (bytes-index bs b)
  (define len (bytes-length bs))
  (let loop ((i 0))
    (cond [(= i len) #f]
          [(eqv? (bytes-ref bs i) b) i]
          [else (loop (+ i 1))])))

(spawn (during/spawn (observe (tcp-channel-line $src $dst _))
         (field [buffer #""])
         (on (message (tcp-channel src dst $bs))
             (buffer (bytes-append (buffer) bs)))
         (begin/dataflow
           (define newline-pos (bytes-index (buffer) (char->integer #\newline)))
           (when newline-pos
             (define line (subbytes (buffer) 0 newline-pos))
             (buffer (subbytes (buffer) (+ newline-pos 1)))
             (send! (tcp-channel-line src dst line))))))
