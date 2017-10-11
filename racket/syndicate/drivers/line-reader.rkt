#lang syndicate

(provide (struct-out tcp-channel-line))

(require "tcp.rkt")
(require "../support/bytes.rkt")

(struct tcp-channel-line (source destination bytes) #:prefab)

(spawn #:name 'line-reader-factory
       (during/spawn (observe (tcp-channel-line $src $dst _))
         #:name `(line-reader ,src ,dst)
         (field [buffer #""])
         (on (message (tcp-channel src dst $bs))
             (buffer (bytes-append (buffer) bs)))
         (begin/dataflow
           (define newline-pos (bytes-index (buffer) (char->integer #\newline)))
           (when newline-pos
             (define line (subbytes (buffer) 0 newline-pos))
             (buffer (subbytes (buffer) (+ newline-pos 1)))
             (send! (tcp-channel-line src dst line))))))
