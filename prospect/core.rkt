#lang racket/base
;; Core implementation of Incremental Network Calculus.

(provide )

(require racket/set)
(require racket/match)
(require (only-in racket/list flatten))
(require "functional-queue.rkt")
(require "route.rkt")
(require "patch.rkt")

;; Events ⊃ Patches
;; Actions ⊃ Events
(struct quit () #:prefab)
(struct spawn (behavior boot) #:prefab)

;; Processes (machine states)
(struct process (interests behavior state) #:transparent)

;; A Behavior is a ((Option Event) Any -> Transition): a function
;; mapping an Event (or, in the #f case, a poll signal) and a
;; Process's current state to a Transition.
;;
;; A Transition is either
;;  - #f, a signal from a Process that it is inert and need not be
;;        scheduled until some Event relevant to it arrives; or,
;;  - a (transition Any (Constreeof Action)), a new Process state to
;;        be held by its World and a sequence of Actions for the World
;;        to take on the transitioning Process's behalf.
(struct transition (state actions) #:transparent)

;; A PID is a Nat.
;; A Label is a PID or 'meta.

;; VM private states
(struct world (next-pid ;; PID
               pending-action-queue ;; (Queueof (Cons Label Action))
               runnable-pids ;; (Setof PID)
               routing-table ;; (Matcherof (Setof Label))
               process-table ;; (HashTable PID Process)
               environment-interests ;; (Matcherof (set 'meta))
               ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event? x) (or (patch? x)))
(define (action? x) (or (event? x) (spawn? x) (quit? x)))

(define (meta-label? x) (eq? x 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ensure-transition v)
  (if (or (not v) (transition? v))
      v
      (raise (exn:fail:contract (format "Expected transition (or #f); got ~v" v)
				(current-continuation-marks)))))

(define (clean-transition t)
  (and t (transition (transition-state t) (clean-actions (transition-actions t)))))

(define (clean-actions actions)
  (filter action? (flatten actions)))

(define (deliver-event e pid p w)
  (invoke-process pid
                  (lambda () ((process-behavior p) e (process-state p)))
                  (lambda (t) (apply-transition pid t w))
                  (lambda (exn) (kill-process pid exn w))))

(define (invoke-process pid thunk k-ok k-exn)
  (define-values (ok? result)
    (with-handlers ([(lambda (exn) #t) (lambda (exn) (values #f exn))])
      (values #t (clean-transition
                  (ensure-transition
                   (with-continuation-mark 'minimart-process pid (thunk)))))))
  (if ok?
      (k-ok result)
      (k-exn result)))

(define (kill-process pid maybe-exn w)
  (define pt (world-process-table w))
  (match (hash-ref pt pid)
    [#f w]
    [(process interests _ _)
     (enqueue-actions (struct-copy world w [process-table (hash-remove pt pid)])
                      pid
                      (list (patch (matcher-empty) interests)))]))

(define (mark-pid-runnable w pid)
  (struct-copy world w [runnable-pids (set-add (world-runnable-pids w) pid)]))

(define (apply-transition pid t w)
  (match t
    [#f w]
    [(transition new-state new-actions)
     (let* ((w (transform-process pid w (lambda (p) (struct-copy process p [state new-state])))))
       (enqueue-actions (mark-pid-runnable w pid) pid new-actions))]))

(define (transform-process pid w fp)
  (define pt (world-process-table w))
  (match (hash-ref pt pid)
    [#f w]
    [p (struct-copy world w [process-table (hash-set pt pid (fp p))])]))

(define (enqueue-actions w label actions)
  (struct-copy world w
    [pending-action-queue
     (queue-append-list (world-pending-action-queue w)
                        (for/list [(a actions)] (cons label a)))]))

(define (spawn-world . boot-actions)
  (spawn world-handle-event
         (lambda () (transition (world 0
                                       (make-queue)
                                       (set)
                                       (matcher-empty)
                                       (hash)
                                       (matcher-empty))
                                '()))))

(define (transition-bind k t0)
  (match-define (transition state0 actions0) t0)
  (match (k state0)
    [#f t0]
    [(transition state1 actions1) (transition state1 (cons actions0 actions1))]))

(define (sequence-transitions t0 . steps)
  (foldl transition-bind t0 steps))

(define (inert? w)
  (and (queue-empty? (world-pending-action-queue w))
       (set-empty? (world-runnable-pids w))))

(define (world-handle-event e w)
  (if (or e (not (inert? w)))
      (sequence-transitions (transition w '())
                            (inject-event e)
                            perform-actions
                            step-children)
      (step-children w)))

(define ((inject-event e) w)
  (match e
    [#f w]
    [(? patch? delta)
     (enqueue-actions w 'meta (list (lift-patch delta)))]))

(define (perform-actions w)
  (for/fold ([wt (transition (struct-copy world w [pending-action-queue (make-queue)]) '())])
      ((entry (in-list (queue->list (world-pending-action-queue w)))))
    (match-define [cons label a] entry)
    (transition-bind (perform-action label a) wt)))

(define ((perform-action label a) w)
  (match a
    [(spawn behavior boot)
     (define new-pid (world-next-pid w))
     (invoke-process new-pid
                     boot
                     (lambda (initial-t)
                       (match-define (transition initial-state initial-actions) initial-t)
                       (define new-p (process (matcher-empty) behavior initial-state))
                       (define new-w
                         (struct-copy world w
                                      [next-pid (+ new-pid 1)]
                                      [process-table
                                       (hash-set (world-process-table w) new-pid new-p)]))
                       (mark-pid-runnable (enqueue-actions new-w new-pid initial-actions)
                                          new-pid))
                     (lambda (exn) (kill-process new-pid exn w)))]
    [(quit) (kill-process label #f w)]
    [(? patch? delta-orig)
     (define p (hash-ref (world-process-table w) label))
     (define old-interests (cond
                             [p (process-interests p)]
                             [(meta-label? label) (world-environment-interests w)]
                             [else (matcher-empty)]))
     (define old-routing-table (world-routing-table w))
     (define delta (limit-patch (label-patch delta-orig label) old-interests))
     (define delta-aggregate (compute-patch-aggregate delta label old-routing-table))
     (define new-routing-table (apply-patch label old-routing-table delta))
     (define affected-pids (compute-affected-pids ...

(define (step-children w)
  (define runnable-pids (world-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; world is inert.
      (transition (for/fold ([w (struct-copy world w [runnable-pids (set)])])
		      [(pid (in-set runnable-pids))]
		    (define p (hash-ref (world-process-table w) pid (lambda () #f)))
		    (if (not p) w (deliver-event #f pid p w)))
		  '())))
