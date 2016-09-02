#lang racket/base

(provide ide-dataspace
         install-ide-dataspace!)

(require racket/async-channel)
(require (only-in racket/list flatten))
(require racket/match)
(require (only-in racket/math pi sqr))
(require racket/set)
(require 2htdp/image)

(require (only-in syndicate seal process-name process-behavior process))
(require (only-in syndicate/dataspace dataspace?))
(require (only-in syndicate/relay relay))
(require syndicate/actor)
(require (only-in syndicate/lang current-ground-dataspace))
(require syndicate/patch)
(require syndicate/protocol/standard-relay)
(require syndicate/ground)
(require syndicate/trace)
(require syndicate/store)
(require syndicate-gl/2d)

(require "hsv.rkt")

(struct view-position (pid pos) #:prefab) ;; assertion
(struct influence (subject object) #:prefab) ;; message

;; A SpringType is one of
;; - tension - only pulls the ends together when stretched
;; - compression - only pushes the ends apart when compressed
;; - both - both tension and compression

(struct spring (type subject object strength length) #:prefab) ;; assertion
(struct push-view (pid delta) #:prefab) ;; message

(define (random-in-range lo hi)
  (+ lo (* (random) (- hi lo))))

(define (tooltip touching? x y w h label-strings0)
  (define label-strings (flatten label-strings0))
  (define label-text (apply above (map (lambda (s) (text s 22 "black")) label-strings)))
  (define label (overlay label-text (empty-scene (+ (image-width label-text) 10)
                                                 (+ (image-height label-text) 10))))
  (define (x-pos window-width)
    (define left (- (x) (image-width label) 10))
    (define right (+ (x) w 10))
    (cond
      [(not (negative? left)) left]
      [(<= (+ right (image-width label)) window-width) right]
      [else (+ (x) (* 1/2 w) (* -1/2 (image-width label)))]))
  (react (define/query-value window-width 0 (inbound (window $w _)) w)
         (assert #:when (touching?)
                 (outbound (simple-sprite -10
                                          (x-pos (window-width))
                                          (+ (y) (* 1/2 h) (- (* 1/2 (image-height label))))
                                          (image-width label)
                                          (image-height label)
                                          label)))))

(define (actor-view name parent-pid pid is-dataspace?)
  (actor #:name (list 'actor-view name pid)

         (field [pos #f])
         (define/query-value win #f (inbound (window $w $h)) (window w h)
           #:on-add (when (not (pos)) (pos (make-rectangular (random-in-range 0 w)
                                                             (random-in-range 0 h)))))

         (define color (color-by-hash pid))
         (define costume (circle (if is-dataspace? 40 20) "solid" color))
         (define extent (make-rectangular (image-width costume) (image-height costume)))

         (define (x) (real-part (pos)))
         (define (y) (imag-part (pos)))

         (define/query-value touching? #f (inbound (touching pid)) #t)
         (on-start (tooltip touching? x y (real-part extent) (imag-part extent)
                            (list (format "~a" name)
                                  (format "~a" pid))))

         (assert #:when (pos) (view-position pid (+ (pos) (* 1/2 extent))))
         (assert #:when (pos)
                 (outbound (simple-sprite #:touchable-id pid
                                          #:touchable-predicate in-unit-circle?
                                          0
                                          (x)
                                          (y)
                                          (real-part extent)
                                          (imag-part extent)
                                          costume)))

         (on (message (push-view pid $delta))
             (when (pos)
               (pos (+ (pos) delta))))

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

(define (compute-link-line start-pos end-pos width)
  (define delta (- end-pos start-pos))
  (define heading (angle delta))
  (define displacement (make-polar 10 (- heading (* pi 1/2))))
  (outbound
   (simple-sprite 1
                  #:rotation (* -180 (/ heading pi))
                  (real-part (+ start-pos displacement))
                  (imag-part (+ start-pos displacement))
                  (magnitude delta)
                  width
                  (rectangle 1 1 "solid" "white"))))

(define (spawn-influence-view-factory)
  (actor #:name 'influence-view-factory
         (field [pairs (set)])
         (on (message (influence $subject $object))
             (define entry (cons subject object))
             (when (not (set-member? (pairs) entry))
               (pairs (set-add (pairs) entry))
               (react
                (stop-when (retracted (view-position subject _)) (pairs (set-remove (pairs) entry)))
                (stop-when (retracted (view-position object _)) (pairs (set-remove (pairs) entry))))
               (actor #:name (list 'influence-view entry)
                      (field [strength 1] [line-width 0])
                      (define/query-value subject-pos #f (view-position subject $p) p)
                      (define/query-value object-pos #f (view-position object $p) p)
                      (stop-when (retracted (view-position subject _)))
                      (stop-when (retracted (view-position object _)))
                      (assert #:when (and (subject-pos) (object-pos) (>= (strength) 0.1))
                              (compute-link-line (subject-pos) (object-pos) (line-width)))
                      (assert #:when (>= (strength) 0.1)
                              (spring 'tension subject object 1 (+ 50 (/ 100 (strength)))))
                      (begin/dataflow
                        (define new-width (min 10 (floor (strength))))
                        (when (not (= new-width (line-width))) (line-width new-width)))
                      (on (message (inbound (frame-event $counter _ _ $target-frame-rate)))
                          (define update-every (inexact->exact (floor (/ target-frame-rate 3))))
                          (when (zero? (modulo counter update-every))
                            (strength (/ (strength) (expt 2 (* update-every (/ target-frame-rate)))))))
                      (on (message (influence subject object))
                          (strength (+ (strength) 1))))))))

(define (spawn-layout-engine)
  (actor #:name 'layout-engine
         (define/query-value win #f (inbound (window $w $h)) (window w h))
         (define/query-hash positions (view-position $pid $pos) pid pos)
         (define/query-set springs ($ s (spring _ _ _ _ _)) s)
         (on #:when (win) (message (inbound (frame-event _ _ _ _)))
             (define midpoint (/ (make-rectangular (window-width (win)) (window-height (win))) 2))
             (for [((this-pid pos) (in-hash (positions)))]
               (send! (push-view this-pid
                                 (* 1/10
                                    (+ (- midpoint pos)
                                       (for/sum [((other-pid other-pos) (in-hash (positions)))
                                                 #:when (not (equal? other-pid this-pid))]
                                         (define delta (- pos other-pos))
                                         (when (< (magnitude delta) 10)
                                           (set! delta (make-polar 10 (random-in-range (- pi) pi))))
                                         (make-polar (/ 1000000 (sqr (magnitude delta)))
                                                     (angle delta)))
                                       (for/sum [(s (in-set (springs)))]
                                         (define other-pid (match s
                                                             [(spring _ p (== this-pid) _ _) p]
                                                             [(spring _ (== this-pid) p _ _) p]
                                                             [_ #f]))
                                         (or (and other-pid
                                                  (not (equal? this-pid other-pid))
                                                  (let ((other-pos (hash-ref (positions) other-pid #f)))
                                                    (and other-pos
                                                         (let ()
                                                           (define heading (angle (- other-pos pos)))
                                                           (define current-length (magnitude (- other-pos pos)))
                                                           (define ideal-length (spring-length s))
                                                           (define force (* (spring-strength s)
                                                                            (- current-length ideal-length)))
                                                           (make-polar ((case (spring-type s)
                                                                          [(both) (lambda (z v) v)]
                                                                          [(tension) max]
                                                                          [(compression) min]) 0 force)
                                                                       heading)))))
                                             0))))))))
         ))

(define (process-is-dataspace? p)
  (match p
    [(process _name _beh (? dataspace? _)) #t]
    [(process _name _beh (relay _ _ _ _ _ (process _inner-name _inner-beh (? dataspace? _)))) #t]
    [_ #f]))

(define ((ide-dataspace) . boot-actions)
  (define from-user-thread-ch (make-async-channel))

  (define user-thread
    (thread (lambda ()
              (with-store ((current-trace-procedures
                            (cons (lambda (n) (async-channel-put from-user-thread-ch n))
                                  (current-trace-procedures))))
                (run-ground boot-actions)))))

  (signal-background-activity! #t)
  (with-store ((current-trace-procedures '()))
    ((2d-dataspace #:label "Syndicate IDE")

     (actor #:name 'user-thread-death-monitor
            (field [user-thread-running? #t])
            (assert #:when (user-thread-running?) 'user-thread-running)
            (on (message (inbound (? frame-event? _)))
                (when (thread-dead? user-thread) (user-thread-running? #f))))

     ;; Ground dataspace
     (actor-view 'ground #f '() #t)

     (actor #:name 'notification-relay
            (on (message (inbound (? frame-event? _)))
                (let loop ()
                  (define n (async-channel-try-get from-user-thread-ch))
                  (match n
                    [#f
                     (void)]
                    [(trace-notification _ new-pid 'spawn (list parent-pid p))
                     (actor-view (process-name p)
                                 (match parent-pid
                                   [(cons 'meta _) #f]
                                   [_ parent-pid])
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

     (spawn-influence-view-factory)
     (spawn-layout-engine)

     ;; (actor #:name 'debug
     ;;        (on (message (? trace-notification? $n))
     ;;            (log-info "INBOUND: ~v --~v--> ~v"
     ;;                      (trace-notification-source n)
     ;;                      (trace-notification-type n)
     ;;                      (trace-notification-sink n))))

     )))

(define install-ide-dataspace!
  (make-keyword-procedure
   (lambda (ks vs . positionals)
     (define installed-dataspace (current-ground-dataspace))
     (current-ground-dataspace (keyword-apply ide-dataspace ks vs positionals)))))
