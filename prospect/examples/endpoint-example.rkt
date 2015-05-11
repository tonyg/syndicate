#lang prospect

(require "../endpoint.rkt")
(require "../drivers/timer.rkt")

(spawn-timer-driver)

(define ((log-it eid stage) e u)
  (log-info "endpoint ~a stage ~a state ~a: ~v" eid stage u e)
  (and e (transition (+ u 1)
                     (if (equal? e (message 2))
                         (if (equal? eid 0)
                             (list (unsub 2) (sub 5) (delete-endpoint 1))
                             (list (unsub 2) (sub 5)))
                         '()))))

(spawn (lambda (e u)
         (when (message? e) (log-info "general: ~v" e))
         #f)
       (void)
       (sub ?)
       (unsub (observe ?))
       (unsub (at-meta ?)))

(spawn-endpoint-group 0
                      (add-endpoint
                       (lambda (eid state)
                         (values (endpoint (log-it eid "pre")
                                           (log-it eid "peri")
                                           (log-it eid "post"))
                                 (transition state
                                             (list (sub 1)
                                                   (sub 2))))))
                      (add-endpoint
                       (lambda (eid state)
                         (values (endpoint (log-it eid "pre")
                                           (log-it eid "peri")
                                           (log-it eid "post"))
                                 (transition state
                                             (list (sub 3)
                                                   (sub 2)))))))

(define (after msec thunk)
  (define id (gensym 'after))
  (if (zero? msec)
      (thunk)
      (list
       (spawn (lambda (e s) (and (message? e) (quit (thunk))))
              (void)
              (sub (timer-expired id ?)))
       (message (set-timer id msec 'relative)))))

(after 100
       (lambda ()
         (list
          (message 0)
          (message 1)
          (message 2)
          (message 3)
          (message 4)
          (message 5)
          (message 6))))

(after 100
       (lambda ()
         (list
          (message 0)
          (message 1)
          (message 2)
          (message 3)
          (message 4)
          (message 5)
          (message 6))))
