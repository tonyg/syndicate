#lang syndicate

(require "../endpoint.rkt")
(require/activate "../drivers/timer.rkt")

(define ((log-it eid) e u)
  (log-info "endpoint ~a state ~a: ~v" eid u e)
  (and e (transition (+ u 1)
                     (if (equal? e (message 2))
                         (if (equal? eid 0)
                             (list (unsub 2) (sub 5) (as-endpoint 1 (delete-endpoint)))
                             (list (unsub 2) (sub 5)))
                         '()))))

(spawn (lambda (e u)
         (when (message? e) (log-info "general: ~v" e))
         #f)
       (void)
       (patch-seq (sub ?)
                  (unsub (observe ?))
                  (unsub (at-meta ?))))

(spawn-endpoint-group 0
                      (add-endpoint
                       (lambda (eid state)
                         (values (log-it eid)
                                 (transition state
                                             (list (sub 1)
                                                   (sub 2))))))
                      (add-endpoint
                       (lambda (eid state)
                         (values (log-it eid)
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
