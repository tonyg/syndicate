#lang racket/base

(provide big-bang-dataspace
         big-bang-dataspace/universe
         (struct-out window)
         (struct-out to-server)
         (struct-out from-server)
         (struct-out tick-event)
         (struct-out key-event)
         (struct-out pad-event)
         (struct-out release-event)
         (struct-out mouse-event)
         (struct-out mouse-state)
         (struct-out active-window)
         update-window
         (all-from-out 2htdp/image))

(require racket/set)
(require racket/match)
(require 2htdp/image)
(require (except-in 2htdp/universe
                    key-event?
                    pad-event?
                    mouse-event?))
(require (only-in 2htdp/private/check-aux SQPORT))
(require (only-in racket/list flatten))

(require "main.rkt")
(require "trie.rkt")

;;---------------------------------------------------------------------------

(struct window (id x y z image) #:transparent) ;; image must be sealed

(struct to-server (message) #:transparent)
(struct from-server (message) #:transparent)
(struct tick-event () #:transparent)
(struct key-event (key window) #:transparent)
(struct pad-event (key window) #:transparent)
(struct release-event (key window) #:transparent)
(struct mouse-event (x y window type) #:transparent)
(struct mouse-state (x y window) #:transparent)
(struct active-window (id) #:transparent)

(define (update-window id x y image #:z [z 0])
  (patch-seq (retract (outbound (window id ? ? ? ?)))
             (assert (outbound (window id x y z (seal image))))))

;;---------------------------------------------------------------------------

(struct bb (proc windows inbound outbound halted? x y) #:transparent)

(define window-projection (?! (window ? ? ? ? ?)))

(define (inject b es)
  (interpret-actions (struct-copy bb b [inbound (append (bb-inbound b)
                                                        es
                                                        (list (update-active-window
                                                               (find-active-window b))))])
                     #f
                     #t))

(define (incorporate-patch b p)
  (define-values (added removed) (patch-project/set/single p window-projection))
  (struct-copy bb b
               [windows (sort (set->list (set-union added
                                                    (set-subtract (list->set (bb-windows b))
                                                                  removed)))
                              (lambda (w1 w2) (< (window-z w1) (window-z w2))))]
               [halted? (if (or (and (bb-halted? b)
                                     (not (trie-lookup (patch-removed p) 'stop #f)))
                                (trie-lookup (patch-added p) 'stop #f))
                            #t
                            #f)]))

(define (deliver b e)
  (clean-transition ((process-behavior (bb-proc b)) e (process-state (bb-proc b)))))

(define (interpret-actions b txn need-poll?)
  (match txn
    [#f ;; inert
     (match (bb-inbound b)
       ['()
        (if need-poll?
            (interpret-actions b (deliver b #f) #f)
            (let ((outbound (reverse (bb-outbound b))))
              (if (null? outbound)
                  b
                  (make-package (struct-copy bb b [outbound '()]) outbound))))]
       [(cons e rest)
        (let ((b (struct-copy bb b [inbound rest])))
          (interpret-actions b (deliver b e) #t))])]
    [(<quit> _ _)
     (struct-copy bb b [halted? #t])]
    [(transition new-state actions)
     (let process-actions ((b (struct-copy bb b
                                           [proc (update-process-state (bb-proc b) new-state)]))
                           (actions actions))
       (match actions
         ['() (interpret-actions b #f #t)]
         [(cons a actions)
          (process-actions (match a
                             [(? patch? p)
                              (incorporate-patch b p)]
                             [(message (to-server sexp))
                              (struct-copy bb b
                                           [outbound (cons sexp (bb-outbound b))])]
                             [_ b])
                           actions)]))]))

(define (inside? mx my x y image)
  (and (>= mx x)
       (>= my y)
       (< (- mx x) (image-width image))
       (< (- my y) (image-height image))))

(define (find-active-window b)
  (define mx (bb-x b))
  (define my (bb-y b))
  (let loop ((ws (bb-windows b)))
    (match ws
      ['() #f]
      [(cons (window id x y _ (seal image)) ws)
       (if (inside? mx my x y image) id (loop ws))])))

(define (render b)
  (for/fold [(scene empty-image)] [(w (bb-windows b))]
    (match-define (window _ x y z (seal image)) w)
    (overlay/xy scene x y image)))

(define (update-active-window active-id)
  (patch-seq (retract (active-window ?))
             (assert (active-window active-id))))

(define-syntax-rule (big-bang-dataspace* boot-actions extra-clause ...)
  (let-values (((proc initial-transition)
                (actor->process+transition (dataspace-actor boot-actions))))
    (big-bang (interpret-actions (bb proc
                                     '()
                                     '()
                                     '()
                                     #f
                                     0
                                     0)
                                 initial-transition
                                 #t)
              (on-tick (lambda (b)
                         (inject b (list (message (tick-event))))))
              (on-key (lambda (b k)
                        (inject b (list (message (key-event k (find-active-window b)))))))
              ;; (on-pad (lambda (b p)
              ;;           (inject b (list (message (pad-event p (find-active-window b)))))))
              (on-release (lambda (b k)
                            (inject b (list (message (release-event k (find-active-window b)))))))
              (on-mouse (lambda (b0 x y e)
                          (define b (struct-copy bb b0 [x x] [y y]))
                          (define active-id (find-active-window b))
                          (inject b (list (patch-seq (retract (mouse-state ? ? ?))
                                                     (assert (mouse-state x y active-id)))
                                          (message (mouse-event x y active-id e))))))
              (stop-when bb-halted?)
              extra-clause ...)))

(define-syntax-rule (big-bang-dataspace** width height exit? boot-actions extra-clause ...)
  (begin
    (if (and width height)
        (big-bang-dataspace* boot-actions (to-draw render width height) extra-clause ...)
        (big-bang-dataspace* boot-actions (to-draw render) extra-clause ...))
    (when exit? (exit 0))))

(define ((big-bang-dataspace #:width [width #f]
                             #:height [height #f]
                             #:exit? [exit? #t])
         . boot-actions)
  (big-bang-dataspace** width height exit? boot-actions))

(define ((big-bang-dataspace/universe #:width [width #f]
                                      #:height [height #f]
                                      #:exit? [exit? #t]
                                      #:register [ip LOCALHOST]
                                      #:port [port-number SQPORT]
                                      #:name [world-name (gensym 'syndicate)])
         . boot-actions)
  (big-bang-dataspace** width height exit? boot-actions
                        (on-receive (lambda (b sexps)
                                      (inject b (for/list ((m sexps)) (message (from-server m))))))
                        (register ip)
                        (port port-number)
                        (name world-name)))
