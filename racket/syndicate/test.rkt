#lang racket

(provide test-module-begin
         (rename-out [illegal-trace trace]))

(module reader syntax/module-reader
  syndicate/test-lang)

(require racket/async-channel)
(require racket/engine)

(require (prefix-in big: "actor-lang.rkt"))
(require "monitor.rkt")
(require "upside-down.rkt")
(require (prefix-in little: (only-in "little-actors/core.rkt" run-with)))
(require (for-syntax syntax/parse))
(require rackunit)

(define-syntax (illegal-trace stx)
  (raise-syntax-error #f "trace: only allowed at top level of a test module" stx))

(begin-for-syntax
  (define (trace->actor+channel trace-stx)
    #`(let ([chan (make-async-channel)])
         (cons (trace-actor #,trace-stx
                            (lambda () (async-channel-put chan #t)))
               chan))))

(define-syntax (test-module-begin stx)
  (define-syntax-class not-trace
    #:datum-literals (trace)
    (pattern (~and _:expr (~not _:trace))))
  (syntax-parse stx
    ;; only allow one trace!
    [(_ (~or (~once t:trace)
             exps:expr)
        ...)
     #'(#%module-begin
        ;; do I need to worry about catching exceptions here?
        (define big-result (run-with-trace big:run-ground t (list (spawn-upside-down exps) ...)))
        (define little-result (run-with-trace little:run-with t '(exps ...)))
        (check-equal? big-result little-result)
        ;; it would be nice to specify false-y traces, too
        (check-true big-result)
        (check-true little-result))]))

(define DEFAULT-FUEL 2000)

(define-syntax-rule (run-with-trace run-ground trc act-exps)
  (let ([chan (make-async-channel)])
    (test-harness run-ground
                  chan
                  (trace-actor trc (lambda () (async-channel-put chan #t)))
                  act-exps
                  DEFAULT-FUEL)))

(define (test-harness run-ground chan trace-act acts [timeout never-evt])
  (define syndicate-thread
    (thread (lambda ()
              (engine-run timeout
                          (engine (lambda (x) (run-ground trace-act acts)))))))
  (define result
    (sync (handle-evt chan
                      (lambda (val) #t))
          (handle-evt syndicate-thread
                      (lambda (val)
                        ;; it's possible one of the final events in the
                        ;; dataspace resulted in an accepting trace and the
                        ;; thread ended at the same time, so the scheduler
                        ;; picked this event. Double check the channel for this
                        ;; case.
                        (async-channel-try-get chan)))))
  (kill-thread syndicate-thread)
  result)
