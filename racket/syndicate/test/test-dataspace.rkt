#lang syndicate

(provide with-test-dataspace
         asserted?
         absent?)

(require racket/async-channel
         (only-in racket/base sleep))
(require "../drivers/repl.rkt"
         "../util.rkt")

(require syntax/parse/define)

(define current-dataspace-thread (make-parameter #f))

(define-syntax-parse-rule (with-test-dataspace (boot ...)
                            body ...)
  (do-with-test-dataspace (lambda () boot ...) (lambda () body ...)))

(define (do-with-test-dataspace boot-thunk body-thunk)
  (define ready-chan (make-async-channel))
  (define spawn-acts (capture-spawn-actions 'test-dataspace (lambda () (boot-thunk) (boot-repl #:when-ready ready-chan))))
  (log-test-info "booting test dataspace")
  (parameterize ([current-custodian (make-custodian)]
                 [current-ground-event-async-channel (make-async-channel)])
    (parameterize ([current-dataspace-thread (thread (lambda ()
                                                       (log-test-info "test dataspace starting")
                                                       (run-ground spawn-acts)
                                                       (log-test-info "test dataspace terminated")))])
    (async-channel-get ready-chan)
    (log-test-info "running with test dataspace")
    (with-handlers ([exn? (lambda (the-exn)
                            (log-test-info "shutting down test dataspace")
                            (custodian-shutdown-all (current-custodian))
                            (raise the-exn))])
      (body-thunk))
    (log-test-info "shutting down test dataspace")
    (custodian-shutdown-all (current-custodian)))))

(define (asserted? pat)
  (trie-non-empty? (do-query pat)))

(define (absent? pat)
  (trie-empty? (do-query pat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging

(define (log-test-info fmt . args)
  (apply printf fmt args)
  (newline))
