#lang syndicate/actor
;; A toy spreadsheet model.

(require racket/set)

(define-namespace-anchor ns)

(struct cell (name value) #:transparent) ;; assertion
(struct set-cell (name expr) #:transparent) ;; message

(define (binding-symbol? s)
  (and (symbol? s)
       (let ((chars (string->list (symbol->string s))))
         (and (andmap char-alphabetic? chars)
              (pair? chars)
              (char-upper-case? (car chars))))))

(define (extract-bindings expr)
  (let walk ((expr expr))
    (match expr
      [(? binding-symbol? b) (set b)]
      [(cons a d) (set-union (walk a) (walk d))]
      [_ (set)])))

(define (non-void-field? f) (not (void? (f))))

(define (cell-expr->actor-expr name expr)
  (define bindings (set->list (extract-bindings expr)))
  `(actor (stop-when (message (set-cell ',name _)))
          (field ,@(for/list [(b bindings)] `[,b (void)]))
          (assert #:when (andmap non-void-field? (list ,@bindings))
                  (cell ',name
                        (let (,@(for/list [(b bindings)] `(,b (,b))))
                          ,expr)))
          ,@(for/list [(b bindings)]
              `(on (asserted (cell ',b $value))
                   (,b value)))))

(actor (on (message (set-cell $name $expr))
           (define actor-expr (cell-expr->actor-expr name expr))
           ;; (local-require racket/pretty) (pretty-print actor-expr)
           (eval actor-expr (namespace-anchor->namespace ns))))

(actor (on (asserted (cell $name $value))
           (printf ">>> ~a ~v\n" name value)
           (flush-output)))

(actor (stop-when (message (inbound 'quit)))
       (on (message (inbound (set-cell $name $expr)))
           (send! (set-cell name expr)))
       (void (thread (lambda ()
                       (let loop ()
                         (define cell-name (read))
                         (if (eof-object? cell-name)
                             (send-ground-message 'quit)
                             (let ((new-expr (read)))
                               (send-ground-message (set-cell cell-name new-expr))
                               (loop))))))))
