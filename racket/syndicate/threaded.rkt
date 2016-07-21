#lang racket/base

(provide spawn-threaded-actor
         actor/thread       ;; \__ once dataspace is split into mux and relay, these two
         dataspace/thread)  ;; /   will be very thin convenience macros over a common impl.

(require racket/match)
(require (for-syntax racket/base))

(require (except-in syndicate dataspace))
(require (only-in syndicate/actor actor dataspace schedule-action!))
(require syndicate/hierarchy)

(struct proxy-state (thd) #:prefab)
(struct thread-transition (quit? actions) #:prefab)

(define (proxy-behaviour e s)
  (match-define (proxy-state thd) s)
  (match e
    [(message (thread-transition #t acs)) (quit acs)]
    [(message (thread-transition #f acs)) (transition s acs)]
    [_
     (when e
       (signal-background-activity! #t)
       (thread-send thd e #f))
     #f]))

(define (spawn-threaded-actor spawn-action-thunk)
  (<spawn> (lambda ()
             (define path (current-actor-path))
             (define thd (thread (lambda () (run-thread path spawn-action-thunk))))
             (thread (lambda ()
                       (sync (thread-dead-evt thd))
                       (send-ground-message (thread-transition #t '()) #:path path)))
             (signal-background-activity! #t)
             (list proxy-behaviour
                   (transition (proxy-state thd) '())
                   'threaded-proxy))))

(define (run-thread actor-path spawn-action-thunk)
  (define actor-path-rev (reverse actor-path))
  (match-define (list (? procedure? behaviour)
                      (? general-transition? initial-transition)
                      _name)
    ((spawn-boot (spawn-action-thunk))))

  (define (put-actions quit? acs)
    (when (not (or (null? acs) (eq? acs #f) (void? acs)))
      (send-ground-message (thread-transition quit? acs) #:path actor-path)))

  (define (process-transition state t)
    (match t
      [(<quit> _exn acs)
       (put-actions #t acs)
       (signal-background-activity! #f)]
      [(transition new-state acs)
       (put-actions #f acs)
       (deliver-event #f new-state)]
      [_
       (await-event state)]))

  (define (deliver-event e state)
    (process-transition state
                        (parameterize ((current-actor-path-rev actor-path-rev))
                          (behaviour e state))))

  (define (await-event state)
    (signal-background-activity! #f)
    (deliver-event (thread-receive) state))

  (process-transition (void) initial-transition))

(define-syntax actor/thread
  (syntax-rules ()
    [(_ body ...)
     (schedule-action!
      (spawn-threaded-actor (lambda () (actor body ...))))]))

(define-syntax dataspace/thread
  (syntax-rules ()
    [(_ body ...)
     (schedule-action!
      (spawn-threaded-actor (lambda () (dataspace body ...))))]))
