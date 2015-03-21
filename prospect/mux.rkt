#lang racket/base
;; General multiplexer.

(provide meta-label?
         (except-out (struct-out mux) mux)
         (rename-out [mux <mux>] [make-mux mux])
         mux-add-stream
         mux-remove-stream
         mux-update-stream
         mux-route-message
         mux-interests-of)

(require racket/set)
(require racket/match)
(require "route.rkt")
(require "patch.rkt")
(require "trace.rkt")

;; A PID is a Nat.
;; A Label is a PID or 'meta.
;; Multiplexer private states
(struct mux (next-pid ;; PID
             routing-table ;; (Matcherof (Setof Label))
             interest-table ;; (HashTable Label Matcher)
             ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meta-label? x) (eq? x 'meta))

(define (make-mux)
  (mux 0 (matcher-empty) (hash)))

(define (mux-add-stream m initial-patch)
  (define new-pid (mux-next-pid m))
  (mux-update-stream (struct-copy mux m [next-pid (+ new-pid 1)])
                     new-pid
                     initial-patch))

(define (mux-remove-stream m label)
  (mux-update-stream m label (patch (matcher-empty) (pattern->matcher #t ?))))

(define (mux-update-stream m label delta-orig)
  (define old-interests (mux-interests-of m label))
  (define delta (limit-patch (label-patch delta-orig (set label)) old-interests))
  (define new-interests (apply-patch old-interests delta))
  (let* ((m (struct-copy mux m
                         [interest-table
                          (if (matcher-empty? new-interests)
                              (hash-remove (mux-interest-table m) label)
                              (hash-set (mux-interest-table m) label new-interests))])))
    ;; CONDITION at this point: delta has been labelled and limited to
    ;; be minimal with respect to existing interests of its label.
    (define old-routing-table (mux-routing-table m))
    (define new-routing-table (apply-patch old-routing-table delta))
    (define delta-aggregate (compute-aggregate-patch delta label old-routing-table))
    (define affected-pids (let ((pids (compute-affected-pids old-routing-table delta)))
                            (set-remove (set-add pids label) 'meta))) ;; TODO: removing meta is weird
    (values (struct-copy mux m [routing-table new-routing-table])
            label
            (for/list [(pid affected-pids)]
              (cond [(equal? pid label)
                     (define feedback
                       (patch-union
                        (patch (biased-intersection new-routing-table (patch-added delta))
                               (biased-intersection old-routing-table (patch-removed delta)))
                        (view-patch delta-aggregate (mux-interests-of m pid))))
                     (cons label feedback)]
                    [else
                     (cons pid (view-patch delta-aggregate (mux-interests-of m pid)))]))
            (and (not (meta-label? label))
                 (drop-patch
                  (compute-aggregate-patch delta label old-routing-table #:remove-meta? #t))))))

(define (compute-affected-pids routing-table delta)
  (define cover (matcher-union (patch-added delta) (patch-removed delta)))
  (matcher-match-matcher cover
                         (matcher-step routing-table struct:observe)
                         #:seed (set)
                         #:combiner (lambda (v1 v2 acc) (set-union v2 acc))
                         #:left-short (lambda (v r acc)
                                        (set-union acc (success-value (matcher-step r EOS))))))

(define (mux-route-message m label body)
  (when (observe? body)
    (log-warning "Stream ~a sent message containing query ~v"
                 (cons label (trace-pid-stack))
                 body))
  (cond
    [(matcher-match-value (mux-routing-table m) body #f) ;; some other stream has declared body
     (values #f '())]
    [(and (not (meta-label? label)) ;; it's from a local process, not envt
          (at-meta? body)) ;; it relates to envt, not local
     (values #t '())]
    [else
     (values #f (set->list (matcher-match-value (mux-routing-table m) (observe body))))]))

(define (mux-interests-of m label)
  (hash-ref (mux-interest-table m) label (matcher-empty)))
