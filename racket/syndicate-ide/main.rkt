#lang racket/base

(provide ide-dataspace
         install-ide-dataspace!)

(require racket/async-channel)
(require racket/match)
(require racket/set)
(require 2htdp/image)

(require (only-in syndicate seal process-behavior process))
(require (only-in syndicate/dataspace dataspace?))
(require (only-in syndicate/relay relay))
(require syndicate/actor)
(require (only-in syndicate/lang current-ground-dataspace))
(require syndicate/patch)
(require syndicate/protocol/standard-relay)
(require syndicate/ground)
(require syndicate/trace)
(require syndicate-gl/2d)

(require "hsv.rkt")

(struct view-position (pid pos) #:prefab) ;; assertion
(struct influence (subject object) #:prefab) ;; message

(define (random-in-range lo hi)
  (+ lo (* (random) (- hi lo))))

(define (coord-top pos extent scale)
  (define half-scale (* 1/2 scale))
  (+ half-scale (- (* half-scale pos) (* 1/2 extent))))

(define (actor-view parent-pid pid is-dataspace?)
  (actor #:name (list 'actor-view pid)

         (field [pos (make-rectangular (random-in-range -1 1) (random-in-range -1 1))])

         (define/query-value win (window 1 1) (inbound (window $w $h)) (window w h))

         (define color (color-by-hash pid))
         (define costume (circle (if is-dataspace? 40 20) "solid" color))
         (define extent (make-rectangular (image-width costume) (image-height costume)))

         (assert (view-position pid (pos)))
         (assert (outbound (simple-sprite 0
                                          (coord-top (real-part (pos))
                                                     (real-part extent)
                                                     (window-width (win)))
                                          (coord-top (imag-part (pos))
                                                     (imag-part extent)
                                                     (window-height (win)))
                                          (real-part extent)
                                          (imag-part extent)
                                          costume)))

         (on (message (trace-notification pid _ 'action _))
             (log-info "~v acting" pid)
             (void))

         (on (message (influence pid _))
             (log-info "~v influencing" pid)
             (void))

         (on (message (influence _ pid))
             (log-info "~v influenced" pid)
             (void))

         (stop-when (message (trace-notification _ pid 'exit _)))))

(define (process-is-dataspace? p)
  (match p
    [(process _name _beh (? dataspace? _)) #t]
    [(process _name _beh (relay _ _ _ _ _ (process _inner-name _inner-beh (? dataspace? _)))) #t]
    [_ #f]))

(define ((ide-dataspace) . boot-actions)
  (define from-user-thread-ch (make-async-channel))

  (define user-thread
    (thread (lambda ()
              (parameterize ((current-trace-procedures
                              (cons (lambda (n) (async-channel-put from-user-thread-ch n))
                                    (current-trace-procedures))))
                (run-ground boot-actions)))))

  (signal-background-activity! #t)
  (parameterize ((current-trace-procedures '()))
    ((2d-dataspace #:label "Syndicate IDE")

     (actor #:name 'user-thread-death-monitor
            (field [user-thread-running? #t])
            (assert #:when (user-thread-running?) 'user-thread-running)
            (on (message (inbound (? frame-event? _)))
                (when (thread-dead? user-thread) (user-thread-running? #f))))

     (actor #:name 'notification-relay
            (on (message (inbound (? frame-event? _)))
                (let loop ()
                  (define n (async-channel-try-get from-user-thread-ch))
                  (match n
                    [#f
                     (void)]
                    [(trace-notification _ new-pid 'spawn (list parent-pid p))
                     (actor-view parent-pid
                                 new-pid
                                 (process-is-dataspace? p))]
                    [(trace-notification s o 'influence (? patch? p))
                     (for [(source-pid (in-set (extract-patch-pids p)))]
                       (send! (influence (cons source-pid (cdr s)) o)))]
                    [(trace-notification s o 'influence (message _))
                     (send! (influence s o))]
                    [(? trace-notification? n)
                     (send! n)])
                  (when n (loop)))))

     (actor #:name 'quit-listener
            (on (message (inbound (key-event #\q #t _)))
                (assert! (outbound 'stop))))

     (actor #:name 'debug
            (on (message (? trace-notification? $n))
                (log-info "INBOUND: ~v --~v--> ~v"
                          (trace-notification-source n)
                          (trace-notification-type n)
                          (trace-notification-sink n))))
     )))

(define install-ide-dataspace!
  (make-keyword-procedure
   (lambda (ks vs . positionals)
     (define installed-dataspace (current-ground-dataspace))
     (current-ground-dataspace (keyword-apply ide-dataspace ks vs positionals)))))
