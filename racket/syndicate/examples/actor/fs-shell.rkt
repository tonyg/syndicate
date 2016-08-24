#lang syndicate/actor
;; Simple "shell" or REPL, used by the file-system demos.

(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))

(require/activate syndicate/drivers/timer)
(require/activate "fs-protocol.rkt")

(define (sleep sec)
  (define timer-id (gensym 'sleep))
  (until (message (timer-expired timer-id _))
         (on-start (send! (set-timer timer-id (* sec 1000.0) 'relative)))))

;; Shell
(let ((e (read-bytes-line-evt (current-input-port) 'any)))
  (define (print-prompt)
    (printf "> ")
    (flush-output))
  (actor (field [reader-count 0])
         (on-start (print-prompt))
         (stop-when (message (inbound (external-event e (list (? eof-object? _))))))
         (on (message (inbound (external-event e (list (? bytes? $bs)))))
             (match (string-split (string-trim (bytes->string/utf-8 bs)))
               [(list "open" name)
                (define reader-id (reader-count))
                (reader-count (+ (reader-count) 1))
                (actor (on-start (printf "Reader ~a opening file ~v.\n" reader-id name))
                       (stop-when (message `(stop-watching ,name)))
                       (on (asserted (file name $contents))
                           (printf "Reader ~a sees that ~v contains: ~v\n"
                                   reader-id
                                   name
                                   contents))
                       (on-stop (printf "Reader ~a closing file ~v.\n" reader-id name)))]
               [(list "close" name)
                (send! `(stop-watching ,name))]
               [(list* "write" name words)
                (send! (save (file name words)))]
               [(list "delete" name)
                (send! (delete name))]
               [_
                (printf "I'm afraid I didn't understand that.\n")
                (printf "Try: open filename\n")
                (printf "     close filename\n")
                (printf "     write filename some text goes here\n")
                (printf "     delete filename\n")])
             (sleep 0.1)
             (print-prompt))))
