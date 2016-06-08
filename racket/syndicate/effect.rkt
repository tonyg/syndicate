#lang racket/base
;; Simple effect system.

;; Should `with-effect` be called `with-effect-handler` (or `with-effect-handlers`)?

(provide (except-out (struct-out effect-tag) effect-tag)
         make-effect-tag
         effect-available?
         perform
         perform/abort
         handle*
         with-effect)

(require racket/control)
(require racket/match)

(struct effect-tag (name prompt) #:transparent)

(define (make-effect-tag name) (effect-tag name (make-continuation-prompt-tag name)))

(struct instruction (action k))
(struct result (values))

(define (effect-available? tag)
  (continuation-prompt-available? (effect-tag-prompt tag)))

(define (ensure-effect-available! who tag action)
  (unless (effect-available? tag)
    (error who
           "Attempt to invoke action ~v in effect ~a with no handler installed."
           action
           (effect-tag-name tag))))

(define ((perform tag) action)
  (ensure-effect-available! 'perform tag action)
  (define p (effect-tag-prompt tag))
  (call-with-composable-continuation
   (lambda (k) (abort/cc p (lambda () (instruction action k))))
   p))

(define ((perform/abort tag) action)
  (ensure-effect-available! 'perform/abort tag action)
  (abort/cc (effect-tag-prompt tag) (lambda () (instruction action #f))))

(define (handle* shallow? tag body-thunk action-proc result-proc)
  (define p (effect-tag-prompt tag))
  (let run ((body-thunk body-thunk))
    (call-with-values (lambda ()
                        (call-with-continuation-prompt
                         (lambda () (call-with-values
                                     body-thunk
                                     (lambda results
                                       (abort/cc p (lambda () (result results))))))
                         p))
                      (match-lambda
                        [(instruction action k)
                         (action-proc action
                                      (if shallow?
                                          k
                                          (lambda vs
                                            (run (lambda () (apply k vs))))))]
                        [(result vs)
                         (apply result-proc vs)]))))

(define-syntax shallow-or-deep
  (syntax-rules ()
    ((shallow-or-deep #:shallow) #t)
    ((shallow-or-deep #:deep) #f)))

(define-syntax with-effect
  (syntax-rules ()
    ((with-effect sd tag-exp k-var (clause ...) effectful-exp)
     (with-effect sd tag-exp k-var (clause ...) effectful-exp #:return values))
    ((with-effect sd tag-exp k-var (clause ...) effectful-exp #:return result-proc)
     (handle* (shallow-or-deep sd)
              tag-exp
              (lambda () effectful-exp)
              (lambda (action k-var) (match action clause ...))
              result-proc))))

(module+ test
  (require rackunit)

  (struct get ())
  (struct set (v))

  (define cell-effect (make-effect-tag 'cell))

  (define do! (perform cell-effect))

  (define (with-shallow-cell-effect initial-value thunk)
    (let loop ((value initial-value) (thunk thunk))
      (with-effect #:shallow cell-effect k
        ([(get) (loop value (lambda () (k value)))]
         [(set v) (loop v (lambda () (k value)))])
        (thunk))))

  (define (with-deep-cell-effect initial-value thunk)
    ((with-effect #:deep cell-effect k
       ([(get) (lambda (s) ((k s) s))]
        [(set v) (lambda (s) ((k s) v))])
       (thunk)
       #:return (lambda (v) (lambda (s) v)))
     initial-value))

  (define (tracing-cell-effect initial-value thunk)
    (struct finish (v))
    (let loop ((trace '())
               (value initial-value)
               (thunk (lambda () ((perform cell-effect) (finish (thunk))))))
      (with-effect #:shallow cell-effect k
        ([(get) (loop (cons `get trace) value (lambda () (k value)))]
         [(set v) (loop (cons `(set ,v) trace) v (lambda () (k value)))]
         [(finish v) (reverse (cons `(result ,v) trace))])
        (thunk))))

  (define (tracing-cell-effect2 initial-value thunk)
    (let loop ((trace '())
               (value initial-value)
               (thunk thunk))
      (with-effect #:shallow cell-effect k
        ([(get) (loop (cons `get trace) value (lambda () (k value)))]
         [(set v) (loop (cons `(set ,v) trace) v (lambda () (k value)))])
        (thunk)
        #:return (lambda (v) (reverse (cons `(result ,v) trace))))))

  (define (tracing-cell-effect3 initial-value thunk)
    (with-shallow-cell-effect '()
      (lambda ()
        (define final
          (let loop ((value initial-value)
                     (thunk thunk))
            (with-effect #:shallow cell-effect k
              ([(get)
                (do! (set (cons `get (do! (get)))))
                (loop value (lambda () (k value)))]
               [(set v)
                (do! (set (cons `(set ,v) (do! (get)))))
                (loop v (lambda () (k value)))])
              (thunk))))
        (reverse (cons `(result ,final) (do! (get)))))))

  (define (do-something)
    (do! (set (+ (do! (get)) 1)))
    (list (do! (get))
          (begin (do! (set (+ (do! (get)) 1)))
                 (do! (get)))))

  (check-equal? (with-shallow-cell-effect 0 do-something)
                (list 1 2))
  (check-equal? (tracing-cell-effect 0 do-something)
                `(get (set 1) get get (set 2) get (result ,(list 1 2))))
  (check-equal? (tracing-cell-effect2 0 do-something)
                `(get (set 1) get get (set 2) get (result ,(list 1 2))))
  (check-equal? (tracing-cell-effect3 0 do-something)
                `(get (set 1) get get (set 2) get (result ,(list 1 2))))

  (check-equal? (with-deep-cell-effect 0 do-something)
                (list 1 2)))
