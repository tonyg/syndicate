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

(require/activate "drivers/timestate.rkt")

(define-syntax-rule (supervise expr ...)
  (supervise* (lambda () expr ...)))

(define (supervise* actor-producing-thunk)
  (with-store [(current-action-transformer (supervise-spawn (current-action-transformer)))]
    (actor-producing-thunk)))

(define ((supervise-spawn previous-action-transformer) ac)
  (match (previous-action-transformer ac)
    [(? spawn? s) (supervise** s)]
    [other other]))

(define (supervise** s)
  (make-spawn (lambda ()
                ;; TODO: Consider closing supervisor-main over s
                ;; rather than over st0 and acs. That way, effects
                ;; from spawn->process+transition will reappear at
                ;; each reboot.
                ;;
                ;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                ;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                ;; BUG: Exception thrown inside
                ;; spawn->process+transition will kill the supervisor!
                ;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                ;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                ;;
                (define-values (proc initial-transition) (spawn->process+transition s))
                (match initial-transition
                  [(transition st0 acs)
                   (list actor-behavior
                         (boot-actor (lambda ()
                                       (react
                                        (supervisor-main proc st0 acs))))
                         (list 'supervised (process-name proc)))]
                  [other
                   (list (process-behavior proc)
                         other
                         (list 'supervised (process-name proc)))]))))

(define (supervisor-main proc boot-state boot-actions)
  (field [done? #f])
  (stop-when (rising-edge (done?)))

  (field [should-run? #f]
         [ready? #f])

  (define inner-handle-event (process-behavior proc))

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
      (log-error "Supervised process ~v ~a reached max restart intensity. Sleeping for ~a seconds"
                 (process-name proc)
                 (current-actor-path)
                 sleep-time)
      (sleep sleep-time)))

  (on (rising-edge (not (ready?)))
      (should-run? #f)
      (retract! ?)
      (flush!)
      (should-run? #t))

  (on (rising-edge (should-run?))
      (react (stop-when (rising-edge (not (should-run?))))
             (field [inner-state boot-state])
             (on-start (perform-actions! boot-actions)
                       (ready? #t))
             (on-event
              [e (with-handlers ([(lambda (exn) #t)
                                  (lambda (e)
                                    (log-error "Supervised process ~v ~a died with exception:\n~a"
                                               (process-name proc)
                                               (current-actor-path)
                                               (if (exn? e)
                                                   (exn->string e)
                                                   (format "~v" e)))
                                    (add-restart!)
                                    (ready? #f))])
                   (match (inner-handle-event e (inner-state))
                     [#f (void)] ;; N.B. TODO: Polling (event of #f)
                     ;; will never reach the inner actor, since
                     ;; actor-behavior doesn't bother executing anything
                     ;; if it is given #f.
                     [(<quit> _ acs) (perform-actions! acs) (done? #t)]
                     ;; N.B. TODO: what to do with the exception carried
                     ;; in the quit struct?
                     [(transition st acs) (perform-actions! acs) (inner-state st)]))]))))

;; BROKENish APPROACH: See uni.org (search for 30048bda-b5c2-11e6-afe4-73eb3e5180cc)

;; (define (supervisor-main proc boot-state boot-actions)
;;   (field [done? #f])
;;   (stop-when (rising-edge (done?)))

;;   (log-info "ORIGINAL: ~v ~v" (process-name proc) boot-state)

;;   (define inner-handle-event (process-behavior proc))
;;   (field [inner-state boot-state] [running? #t])
;;   (on-start (perform-actions! boot-actions))
;;   (define (reboot!)
;;     (log-info "--------------------------------------------------")
;;     (running? #f)
;;     (retract! ?)
;;     ;; (flush!)
;;     (inner-state boot-state)
;;     (log-info "--------------------------------------------------")
;;     (running? #t)
;;     (log-info "REBOOT: ~v ~v" (process-name proc) boot-state)
;;     (perform-actions! boot-actions))

;;   (define intensity 1)
;;   (define period 5000) ;; milliseconds
;;   (define sleep-time 10) ;; seconds
;;   (field [restarts '()])

;;   (define (add-restart!)
;;     (define now (current-inexact-milliseconds))
;;     (define oldest-to-keep (- now period))
;;     (restarts (filter (lambda (r) (>= r oldest-to-keep))
;;                       (cons (current-inexact-milliseconds) (restarts))))
;;     (when (> (length (restarts)) intensity)
;;       (log-error "Supervised process ~v ~a reached max restart intensity. Sleeping for ~a seconds"
;;                  (process-name proc)
;;                  (current-actor-path)
;;                  sleep-time)
;;       (sleep sleep-time)))

;;   (on-event [e (when (running?)
;;                  (with-handlers ([(lambda (exn) #t)
;;                                   (lambda (e)
;;                                     (log-error "Supervised process ~v ~a died with exception:\n~a"
;;                                                (process-name proc)
;;                                                (current-actor-path)
;;                                                (if (exn? e)
;;                                                    (exn->string e)
;;                                                    (format "~v" e)))
;;                                     (add-restart!)
;;                                     (reboot!))])
;;                    (match (inner-handle-event e (inner-state))
;;                      [#f (void)] ;; N.B. TODO: Polling (event of #f)
;;                      ;; will never reach the inner actor, since
;;                      ;; actor-behavior doesn't bother executing anything
;;                      ;; if it is given #f.
;;                      [(<quit> _ acs) (perform-actions! acs) (done? #t)]
;;                      ;; N.B. TODO: what to do with the exception carried
;;                      ;; in the quit struct?
;;                      [(transition st acs) (perform-actions! acs) (inner-state st)])))]))
