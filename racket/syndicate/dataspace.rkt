#lang racket/base
;; Dataspaces without configured relaying.

(provide (struct-out dataspace)
         make-dataspace
         dataspace-actor
         make-dataspace-actor
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

;; Sentinel
(define missing-process (process #f #f #f))

;; VM private states
(struct dataspace (mux ;; Multiplexer
                   pending-action-queue ;; (Queueof (Cons Label (U Action 'quit)))
                   runnable-pids ;; (Setof PID)
                   process-table ;; (HashTable PID Process)
                   )
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print w [p (current-output-port)])
     (pretty-print-dataspace w p))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (send-event origin-pid e pid w)
  (match-define (and the-process (process process-name behavior old-state))
    (hash-ref (dataspace-process-table w) pid missing-process))
  (if (not behavior)
      w
      (begin
        (when origin-pid (trace-causal-influence origin-pid pid e))
        (trace-event-consumed pid e)
        (trace-turn-begin pid the-process)
        (invoke-process pid
                        (lambda () (clean-transition (ensure-transition (behavior e old-state))))
                        (match-lambda
                          [#f
                           (trace-turn-end pid the-process)
                           w]
                          [(and q (<quit> exn final-actions))
                           (trace-turn-end pid the-process)
                           (trace-actor-exit pid exn)
                           (enqueue-actions (disable-process pid exn w) pid (append final-actions
                                                                                    (list 'quit)))]
                          [(and t (transition new-state new-actions))
                           (trace-turn-end pid (process process-name behavior new-state))
                           (enqueue-actions (mark-pid-runnable (update-state w pid new-state) pid)
                                            pid
                                            new-actions)])
                        (lambda (exn)
                          (trace-turn-end pid the-process)
                          (trace-actor-exit pid exn)
                          (enqueue-actions (disable-process pid exn w) pid (list 'quit)))))))

(define (update-process-entry w pid f)
  (define old-pt (dataspace-process-table w))
  (match (hash-ref old-pt pid #f)
    [#f w]
    [old-p (struct-copy dataspace w [process-table (hash-set old-pt pid (f old-p))])]))

(define (update-state w pid s)
  (update-process-entry w pid (lambda (p) (update-process-state p s))))

(define (send-event/guard origin-pid e pid w)
  (if (patch-empty? e)
      w
      (send-event origin-pid e pid w)))

(define (disable-process pid exn w)
  (when exn
    (log-error "Process ~v ~a died with exception:\n~a"
               (process-name (hash-ref (dataspace-process-table w) pid missing-process))
               (append (current-actor-path) (list pid))
               (exn->string exn)))
  ;; We leave a "tombstone", just the process name, until the 'quit pseudoaction takes effect.
  (update-process-entry w pid (lambda (p) (process (process-name p) #f #f))))

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

(define-syntax dataspace-actor
  (syntax-rules ()
    [(dataspace-actor #:name name-exp boot-action ...)
     (spawn-standard-relay
      (make-dataspace-actor #:name name-exp (lambda () (list boot-action ...))))]
    [(dataspace-actor boot-action ...)
     (spawn-standard-relay
      (make-dataspace-actor (lambda () (list boot-action ...))))]))

(define (make-dataspace boot-actions)
  (dataspace (mux)
             (list->queue (for/list ((a (in-list (clean-actions boot-actions)))) (cons 'meta a)))
             (set)
             (hash)))

(define (make-dataspace-actor #:name [name #f] boot-actions-thunk)
  (<actor> (lambda ()
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
    (when (or (event? a) (eq? a 'quit)) (trace-action-produced label a))
    (define wt1 (transition-bind (perform-action label a) wt))
    wt1))

(define ((perform-action label a) w)
  (match a
    [(<actor> boot)
     (invoke-process (mux-next-pid (dataspace-mux w)) ;; anticipate pid allocation
                     (lambda ()
                       (match (boot)
                         [(and results (list (? procedure?) (? general-transition?) _))
                          results]
                         [other
                          (error 'actor
                                 "actor boot procedure must yield boot spec; received ~v"
                                 other)]))
                     (lambda (results)
                       (match-define (list behavior initial-transition name) results)
                       (create-process label w behavior initial-transition name))
                     (lambda (exn)
                       (log-error "Spawned process in dataspace ~a died with exception:\n~a"
                                  (current-actor-path)
                                  (exn->string exn))
                       (transition w '())))]
    ['quit
     (define-values (new-mux _label delta delta-aggregate)
       (mux-remove-stream (dataspace-mux w) label))
     ;; Clean up the "tombstone" left for us by disable-process
     (let ((w (struct-copy dataspace w
                           [process-table (hash-remove (dataspace-process-table w) label)])))
       (deliver-patches w new-mux label delta delta-aggregate))]
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
     (transition (for/fold [(w w)] [(pid (in-list affected-pids))] (send-event label m pid w))
                 (and meta-affected? m))]
    [(targeted-event (cons pid remaining-path) e)
     (transition (send-event/guard label (target-event remaining-path e) pid w) '())]))

(define (create-process parent-label w behavior initial-transition name)
  (if (not initial-transition)
      (transition w '()) ;; Uh, ok
      (let ()
        (define-values (postprocess initial-state initial-actions)
          (match (clean-transition initial-transition)
            [(and q (<quit> exn initial-actions0))
             (values (lambda (w pid)
                       (trace-actor-spawn parent-label pid (process name behavior (void)))
                       (trace-actor-exit pid exn)
                       (disable-process pid exn w))
                     #f
                     (append initial-actions0 (list 'quit)))]
            [(and t (transition initial-state initial-actions0))
             (values (lambda (w pid)
                       (trace-actor-spawn parent-label pid (process name behavior initial-state))
                       (mark-pid-runnable w pid))
                     initial-state
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
                                                        (process name
                                                                 behavior
                                                                 initial-state))]))
               (w (enqueue-actions (postprocess w new-pid) new-pid remaining-initial-actions)))
          (trace-action-produced new-pid initial-patch)
          (deliver-patches w new-mux new-pid delta delta-aggregate)))))

(define (deliver-patches w new-mux acting-label delta delta-aggregate)
  (define-values (patches meta-action)
    (compute-patches (dataspace-mux w) new-mux acting-label delta delta-aggregate))
  (transition (for/fold [(w (struct-copy dataspace w [mux new-mux]))]
                        [(entry (in-list patches))]
                (match-define (cons label event) entry)
                (send-event/guard acting-label event label w))
              (and (patch-non-empty? meta-action) meta-action)))

(define (step-children w)
  (define runnable-pids (dataspace-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; dataspace is inert.
      (transition (for/fold [(w (struct-copy dataspace w [runnable-pids (set)]))]
                            [(pid (in-set runnable-pids))]
                    (send-event #f #f pid w))
		  '())))

(define (pretty-print-dataspace w [p (current-output-port)])
  (match-define (dataspace mux qs runnable process-table) w)
  (fprintf p "DATASPACE:\n")
  (fprintf p " - ~a queued actions\n" (queue-length qs))
  (fprintf p " - ~a runnable pids ~a\n" (set-count runnable) (set->list runnable))
  (fprintf p " - ~a live processes\n" (hash-count process-table))
  (fprintf p " - ")
  (display (indented-port-output 3 (lambda (p) (syndicate-pretty-print mux p)) #:first-line? #f) p)
  (newline p)
  (for ([pid (set-union (hash-keys (mux-interest-table mux)) (hash-keys process-table))])
    (define i (hash-ref process-table pid missing-process))
    (fprintf p " ---- process ~a, name ~v, behavior ~v, STATE:\n"
             pid
             (process-name i)
             (process-behavior i))
    (display (indented-port-output 6 (lambda (p) (syndicate-pretty-print (process-state i) p))) p)
    (newline p)
    (fprintf p "      process ~a, name ~v, behavior ~v, CLAIMS:\n"
             pid
             (process-name i)
             (process-behavior i))
    (display (indented-port-output 6 (lambda (p)
                                       (pretty-print-trie (mux-interests-of mux pid) p)))
             p)
    (newline p)))
