#lang racket/base

(provide tui-dataspace
         install-tui-dataspace!)

(require racket/async-channel)
(require racket/match)
(require racket/set)
(require (only-in racket/string string-replace))

(require (only-in syndicate seal process-name process-behavior process))
(require (only-in syndicate/dataspace dataspace?))
(require (only-in syndicate/relay relay))
(require (only-in syndicate/lang current-ground-dataspace))
(require syndicate/patch)
(require syndicate/ground)
(require syndicate/trace)
(require syndicate/store)

(require "display.rkt")
(require "display-terminal.rkt")

(define (process-is-dataspace? p)
  (match p
    [(process _name _beh (? dataspace? _)) #t]
    [(process _name _beh (relay _ _ _ _ _ (process _inner-name _inner-beh (? dataspace? _)))) #t]
    [_ #f]))

(define ((tui-dataspace) . boot-actions)
  (define from-user-thread-ch (make-async-channel))

  (define user-thread
    (thread (lambda ()
              (with-store ((current-trace-procedures
                            (cons (lambda (n) (async-channel-put from-user-thread-ch n))
                                  (current-trace-procedures))))
                (run-ground boot-actions)))))

  (signal-background-activity! #t)
  (define tty (default-tty))

  (define (dump x)
    (tty-display tty (string-replace (format "~v\n" x) "\n" "\r\n")))

  (let loop ()
    (tty-flush tty)
    (sync (handle-evt from-user-thread-ch
                      (lambda (n)
                        (dump n)
                        (loop)))
          (handle-evt (tty-next-key-evt tty)
                      (lambda (k)
                        (match k
                          [(key #\q (== (set))) (void)]
                          [_
                           (dump k)
                           (loop)]))))))

(define install-tui-dataspace!
  (make-keyword-procedure
   (lambda (ks vs . positionals)
     (define installed-dataspace (current-ground-dataspace))
     (current-ground-dataspace (keyword-apply tui-dataspace ks vs positionals)))))
