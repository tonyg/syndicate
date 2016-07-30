#lang racket/base
;; Dataspaces without configured relaying.

(provide (struct-out dataspace)
         make-dataspace
         spawn-dataspace
         make-spawn-dataspace
         dataspace-handle-event
         pretty-print-dataspace)

(require racket/set)
(require racket/match)
(require "functional-queue.rkt")
(require "trie.rkt")
(require "patch.rkt")
(require "hierarchy.rkt")
(require "trace.rkt")
(require "mux.rkt")
(require "pretty.rkt")
(require "core.rkt")
(require "protocol/standard-relay.rkt")

;; Long-lived process data: (process-info Any Behavior)
(struct process-info (name behavior) #:transparent)

;; Sentinel
(define missing-process-info (process-info #f #f))

;; VM private states
(struct dataspace (mux ;; Multiplexer
                   pending-action-queue ;; (Queueof (Cons Label (U Action 'quit)))
                   runnable-pids ;; (Setof PID)
                   process-table ;; (HashTable PID ProcessInfo)
                   states ;; (HashTable PID Any)
                   )
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print w [p (current-output-port)])
     (pretty-print-dataspace w p))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (send-event e pid w)
  (define behavior (process-info-behavior
                    (hash-ref (dataspace-process-table w) pid missing-process-info)))
  (define old-state (hash-ref (dataspace-states w) pid #f))
  (if (not behavior)
      w
      (begin
        (trace-process-step e pid behavior old-state)
        (invoke-process pid
                        (lambda () (clean-transition (ensure-transition (behavior e old-state))))
                        (match-lambda
                          [#f w]
                          [(and q (<quit> exn final-actions))
                           (trace-process-step-result e pid behavior old-state exn q)
                           (enqueue-actions (disable-process pid exn w) pid (append final-actions
                                                                                    (list 'quit)))]
                          [(and t (transition new-state new-actions))
                           (trace-process-step-result e pid behavior old-state #f t)
                           (enqueue-actions (mark-pid-runnable (update-state w pid new-state) pid)
                                            pid
                                            new-actions)])
                        (lambda (exn)
                          (trace-process-step-result e pid behavior old-state exn #f)
                          (enqueue-actions (disable-process pid exn w) pid (list 'quit)))))))

(define (update-state w pid s)
  (struct-copy dataspace w [states (hash-set (dataspace-states w) pid s)]))

(define (send-event/guard e pid w)
  (if (patch-empty? e)
      w
      (send-event e pid w)))

(define (disable-process pid exn w)
  (when exn
    (log-error "Process ~v ~a died with exception:\n~a"
               (process-info-name (hash-ref (dataspace-process-table w) pid missing-process-info))
               (append (current-actor-path) (list pid))
               (exn->string exn)))
  (struct-copy dataspace w
               [process-table (hash-remove (dataspace-process-table w) pid)]
               [states (hash-remove (dataspace-states w) pid)]))

(define (invoke-process pid thunk k-ok k-exn)
  (define-values (ok? result)
    (call/extended-actor-path
     pid
     (lambda ()
       (with-handlers ([(lambda (exn) #t) (lambda (exn) (values #f exn))])
         (values #t (with-continuation-mark 'minimart-process pid (thunk)))))))
  (if ok?
      (k-ok result)
      (k-exn result)))

(define (mark-pid-runnable w pid)
  (struct-copy dataspace w [runnable-pids (set-add (dataspace-runnable-pids w) pid)]))

(define (enqueue-actions w label actions)
  (struct-copy dataspace w
    [pending-action-queue
     (queue-append-list (dataspace-pending-action-queue w)
                        (for/list [(a actions)] (cons label a)))]))

(define-syntax spawn-dataspace
  (syntax-rules ()
    [(spawn-dataspace #:name name-exp boot-action ...)
     (spawn-standard-relay
      (make-spawn-dataspace #:name name-exp (lambda () (list boot-action ...))))]
    [(spawn-dataspace boot-action ...)
     (spawn-standard-relay
      (make-spawn-dataspace (lambda () (list boot-action ...))))]))

(define (make-dataspace boot-actions)
  (dataspace (mux)
             (list->queue (for/list ((a (in-list (clean-actions boot-actions)))) (cons 'meta a)))
             (set)
             (hash)
             (hash)))

(define (make-spawn-dataspace #:name [name #f] boot-actions-thunk)
  (<spawn> (lambda ()
             (list dataspace-handle-event
                   (transition (make-dataspace (boot-actions-thunk)) '())
                   name))))

(define (inert? w)
  (and (queue-empty? (dataspace-pending-action-queue w))
       (set-empty? (dataspace-runnable-pids w))))

(define (dataspace-handle-event e w)
  (if (or e (not (inert? w)))
      (sequence-transitions (transition w '())
                            (inject-event e)
                            perform-actions
                            (lambda (w) (or (step-children w) (transition w '()))))
      (step-children w)))

(define ((inject-event e) w)
  (transition (if (not e) w (enqueue-actions w 'meta (list e))) '()))

(define (perform-actions w)
  (for/fold ([wt (transition (struct-copy dataspace w [pending-action-queue (make-queue)]) '())])
      ((entry (in-list (queue->list (dataspace-pending-action-queue w)))))
    #:break (quit? wt) ;; TODO: should a quit action be delayed until the end of the turn?
    (match-define [cons label a] entry)
    (trace-internal-action label a (transition-state wt))
    (define wt1 (transition-bind (perform-action label a) wt))
    (trace-internal-action-result label a (transition-state wt) wt1)
    wt1))

(define ((perform-action label a) w)
  (match a
    [(<spawn> boot)
     (invoke-process (mux-next-pid (dataspace-mux w)) ;; anticipate pid allocation
                     (lambda ()
                       (match (boot)
                         [(and results (list (? procedure?) (? general-transition?) _))
                          results]
                         [other
                          (error 'spawn
                                 "Spawn boot procedure must yield boot spec; received ~v"
                                 other)]))
                     (lambda (results)
                       (match-define (list behavior initial-transition name) results)
                       (create-process w behavior initial-transition name))
                     (lambda (exn)
                       (log-error "Spawned process in dataspace ~a died with exception:\n~a"
                                  (current-actor-path)
                                  (exn->string exn))
                       (transition w '())))]
    ['quit
     (define-values (new-mux _label delta delta-aggregate)
       (mux-remove-stream (dataspace-mux w) label))
     ;; behavior & state in w already removed by disable-process
     (deliver-patches w new-mux label delta delta-aggregate)]
    [(quit-dataspace)
     (quit)]
    [(? patch? delta-orig)
     (define-values (new-mux _label delta delta-aggregate)
       (mux-update-stream (dataspace-mux w) label delta-orig))
     (deliver-patches w new-mux label delta delta-aggregate)]
    [(and m (message body))
     (when (observe? body)
       (log-warning "Stream ~a sent message containing query ~v"
                    (append (current-actor-path) (list label))
                    body))
     (define-values (affected-pids meta-affected?) (mux-route-message (dataspace-mux w) body))
     (transition (for/fold [(w w)] [(pid (in-list affected-pids))] (send-event m pid w))
                 (and meta-affected? m))]
    [(targeted-event (cons pid remaining-path) e)
     (transition (send-event/guard (target-event remaining-path e) pid w) '())]))

(define (create-process w behavior initial-transition name)
  (if (not initial-transition)
      (transition w '()) ;; Uh, ok
      (let ()
        (define-values (postprocess initial-actions)
          (match (clean-transition initial-transition)
            [(and q (<quit> exn initial-actions0))
             (values (lambda (w pid)
                       (trace-process-step-result 'boot pid behavior (void) exn q)
                       (disable-process pid exn w))
                     (append initial-actions0 (list 'quit)))]
            [(and t (transition initial-state initial-actions0))
             (values (lambda (w pid)
                       (trace-process-step-result 'boot pid behavior (void) #f t)
                       (mark-pid-runnable (update-state w pid initial-state) pid))
                     initial-actions0)]))
        (define-values (initial-patch remaining-initial-actions)
          (match initial-actions
            [(cons (? patch? p) rest) (values p rest)]
            [other (values patch-empty other)]))
        (define-values (new-mux new-pid delta delta-aggregate)
          (mux-add-stream (dataspace-mux w) initial-patch))
        (let* ((w (struct-copy dataspace w
                               [process-table (hash-set (dataspace-process-table w)
                                                        new-pid
                                                        (process-info name
                                                                      behavior))]))
               (w (enqueue-actions (postprocess w new-pid) new-pid remaining-initial-actions)))
          (deliver-patches w new-mux new-pid delta delta-aggregate)))))

(define (deliver-patches w new-mux acting-label delta delta-aggregate)
  (define-values (patches meta-action)
    (compute-patches (dataspace-mux w) new-mux acting-label delta delta-aggregate))
  (transition (for/fold [(w (struct-copy dataspace w [mux new-mux]))]
                        [(entry (in-list patches))]
                (match-define (cons label event) entry)
                (send-event/guard event label w))
              (and (patch-non-empty? meta-action) meta-action)))

(define (step-children w)
  (define runnable-pids (dataspace-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; dataspace is inert.
      (transition (for/fold [(w (struct-copy dataspace w [runnable-pids (set)]))]
                            [(pid (in-set runnable-pids))]
                    (send-event #f pid w))
		  '())))

(define (pretty-print-dataspace w [p (current-output-port)])
  (match-define (dataspace mux qs runnable process-table states) w)
  (fprintf p "DATASPACE:\n")
  (fprintf p " - ~a queued actions\n" (queue-length qs))
  (fprintf p " - ~a runnable pids ~a\n" (set-count runnable) (set->list runnable))
  (fprintf p " - ~a live processes\n" (hash-count states))
  (fprintf p " - ")
  (display (indented-port-output 3 (lambda (p) (syndicate-pretty-print mux p)) #:first-line? #f) p)
  (newline p)
  (for ([pid (set-union (hash-keys (mux-interest-table mux)) (hash-keys states))])
    (define i (hash-ref process-table pid missing-process-info))
    (fprintf p " ---- process ~a, name ~v, behavior ~v, STATE:\n"
             pid
             (process-info-name i)
             (process-info-behavior i))
    (define state (hash-ref states pid #f))
    (display (indented-port-output 6 (lambda (p) (syndicate-pretty-print state p))) p)
    (newline p)
    (fprintf p "      process ~a, name ~v, behavior ~v, CLAIMS:\n"
             pid
             (process-info-name i)
             (process-info-behavior i))
    (display (indented-port-output 6 (lambda (p)
                                       (pretty-print-trie (mux-interests-of mux pid) p)))
             p)
    (newline p)))
