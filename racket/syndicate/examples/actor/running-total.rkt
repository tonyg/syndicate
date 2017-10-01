#lang syndicate
;; Terminal I/O demo program.

(require (only-in racket/port read-bytes-line-evt))

(define e (read-bytes-line-evt (current-input-port) 'any))

(spawn (field [total 0])
       (begin/dataflow (printf "The total is ~a.\n" (total)))
       (on-stop (printf "Goodbye!\n"))
       (on (message (inbound (external-event e (list $input))))
           (cond
             [(eof-object? input) (stop-current-facet)]
             [(string->number (bytes->string/utf-8 input)) => (lambda (n) (total (+ (total) n)))]
             [else (void)])))
