#lang racket/base

(provide spawn-threaded-actor
         spawn/thread       ;; \__ once dataspace is split into mux and relay, these three
         spawn*/thread      ;;  |  will be very thin convenience macros over a common impl.
         dataspace/thread)  ;; /

(require racket/match)
(require (for-syntax racket/base))

(require (except-in syndicate dataspace))
(require (only-in syndicate/actor spawn spawn* dataspace schedule-action!))
(require (only-in (submod syndicate/actor implementation-details) capture-actor-actions))
(require syndicate/hierarchy)
(require syndicate/store)

(struct proxy-state (thd) #:prefab)
(struct thread-quit (exn actions) #:prefab)
(struct thread-transition (actions) #:prefab)

(define (proxy-behaviour e s)
  (match-define (proxy-state thd) s)
  (match e
    [(message (thread-transition acs)) (transition s acs)]
    [(message (thread-quit exn acs)) (<quit> exn acs)]
    [_
     (when e
       (signal-background-activity! #t)
       (thread-send thd e #f))
     #f]))

(define (spawn-threaded-actor spawn-action-thunk)
  (match-define (list (<actor> boot-proc initial-assertions))
    (clean-actions (capture-actor-actions spawn-action-thunk)))
  (make-actor (lambda ()
                (define path (current-actor-path))
                (define thd (thread (lambda () (run-thread path boot-proc))))
                (thread (lambda ()
                          (sync (thread-dead-evt thd))
                          (send-ground-message (thread-quit #f '()) #:path path)
                          (signal-background-activity! #f)))
                (signal-background-activity! #t)
                (list proxy-behaviour
                      (transition (proxy-state thd) '())
                      'threaded-proxy))
              initial-assertions))

(define (run-thread actor-path boot-proc)
  (define actor-path-rev (reverse actor-path))

  (define (process-transition proc t)
    (match t
      [(<quit> exn acs)
       (send-ground-message (thread-quit exn acs) #:path actor-path)]
      [(transition new-state acs)
       (when (not (or (null? acs) (eq? acs #f) (void? acs)))
         (send-ground-message (thread-transition acs) #:path actor-path))
       (deliver-event #f (update-process-state proc new-state))]
      [_
       (await-event proc)]))

  (define (deliver-event e proc)
    (process-transition proc
                        (with-store ((current-actor-path-rev actor-path-rev))
                          (with-handlers [((lambda (exn) #t) (lambda (exn) (<quit> exn '())))]
                            ((process-behavior proc) e (process-state proc))))))

  (define (await-event proc)
    (signal-background-activity! #f)
    (deliver-event (thread-receive) proc))

  (call-with-values (lambda () (boot->process+transition boot-proc))
                    process-transition))

(define-syntax spawn/thread
  (syntax-rules ()
    [(_ body ...)
     (schedule-action!
      (spawn-threaded-actor (lambda () (spawn body ...))))]))

(define-syntax spawn*/thread
  (syntax-rules ()
    [(_ body ...)
     (schedule-action!
      (spawn-threaded-actor (lambda () (spawn* body ...))))]))

(define-syntax dataspace/thread
  (syntax-rules ()
    [(_ body ...)
     (schedule-action!
      (spawn-threaded-actor (lambda () (dataspace body ...))))]))
