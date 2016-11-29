#lang syndicate/actor
;; Extremely simple single-actor supervision
;; Vastly simplified compared to the available options in OTP

(provide supervise)

(require racket/exn)
(require "core.rkt")
(require "dataflow.rkt")
(require "hierarchy.rkt")
(require "store.rkt")

(require (submod "actor.rkt" implementation-details))
(require (for-syntax syntax/parse))

(require/activate "drivers/timestate.rkt")

(define-syntax (supervise stx)
  (syntax-parse stx
    [(_ name:actor-name expr ...)
     (syntax/loc stx
       (supervise* (lambda () name.N) (lambda () expr ...)))]))

(define (supervise* supervisor-name-thunk actor-producing-thunk)
  ;; Awkward: the name applies to any and all potential supervisors
  ;; produced by actor spawns in actor-producing-thunk.
  (with-store [(current-action-transformer
                (supervise-spawn supervisor-name-thunk (current-action-transformer)))]
    (actor-producing-thunk)))

(define ((supervise-spawn supervisor-name-thunk previous-action-transformer) ac)
  (match (previous-action-transformer ac)
    [(? spawn? s) (supervise** (or (supervisor-name-thunk) (gensym 'supervisor)) s)]
    [other other]))

(define (supervise** supervisor-name supervisee-spawn-action)
  (actor-action #:name supervisor-name
   (react

    (field [done? #f])
    (stop-when (rising-edge (done?)))

    (field [supervisee-name 'unknown])

    (define intensity 1)
    (define period 5000) ;; milliseconds
    (define sleep-time 10) ;; seconds
    (field [restarts '()])

    (define (add-restart!)
      (define now (current-inexact-milliseconds))
      (define oldest-to-keep (- now period))
      (restarts (filter (lambda (r) (>= r oldest-to-keep))
                        (cons (current-inexact-milliseconds) (restarts))))
      (when (> (length (restarts)) intensity)
        (log-error "Supervised process ~s/~s ~a reached max restart intensity. Sleeping for ~a seconds"
                   supervisor-name
                   (supervisee-name)
                   (current-actor-path)
                   sleep-time)
        (sleep sleep-time)))

    (field [should-run? #f]
           [ok? #f])

    (on (rising-edge (not (ok?)))
        (should-run? #f)
        (ok? #t)
        (retract! ?)
        (flush!)
        (should-run? #t))

    (define (catch-exns thunk k)
      (with-handlers ([(lambda (e) #t)
                       (lambda (e)
                         (log-error "Supervised process ~s/~s ~a died with exception:\n~a"
                                    supervisor-name
                                    (supervisee-name)
                                    (current-actor-path)
                                    (if (exn? e)
                                        (exn->string e)
                                        (format "~v" e)))
                         (add-restart!)
                         (ok? #f))])
        (call-with-values thunk k)))

    (on (rising-edge (should-run?))
        (react (stop-when (rising-edge (not (should-run?))))
               (field [proc #f])

               (define (handle-transition! txn)
                 (match txn
                   [#f
                    ;; N.B. TODO: Polling (event of #f) will never
                    ;; reach the inner actor, since actor-behavior
                    ;; doesn't bother executing anything if it is
                    ;; given #f.
                    (void)]
                   [(<quit> _ acs)
                    (perform-actions! acs)
                    ;; N.B. TODO: what to do with the exception
                    ;; carried in the quit struct?
                    (done? #t)]
                   [(transition st acs)
                    (perform-actions! acs)
                    (proc (update-process-state (proc) st))]))

               (on-start
                (catch-exns
                 (lambda ()
                   (define-values (initial-proc initial-transition)
                     (spawn->process+transition supervisee-spawn-action))
                   (proc initial-proc)
                   (supervisee-name (process-name initial-proc))
                   initial-transition)
                 handle-transition!))

               (on-event
                [e (when (proc)
                     (catch-exns
                      (lambda () ((process-behavior (proc)) e (process-state (proc))))
                      handle-transition!))]))))))
