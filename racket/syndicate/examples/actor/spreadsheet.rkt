#lang syndicate
;; A toy spreadsheet model.

(require syndicate/actor)
(require racket/match)
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

(define (non-void? v) (not (void? v)))

(define (cell-expr->actor-expr name expr)
  (define bindings (set->list (extract-bindings expr)))
  `(actor (until (message (set-cell ',name _))
                 #:collect [,@(for/list [(b bindings)] `(,b (void)))]
                 (assert #:when (andmap non-void? (list ,@bindings)) (cell ',name ,expr))
                 ,@(for/list [(b bindings)]
                     `(on (asserted (cell ',b $value))
                          (values ,@(for/list [(b1 bindings)]
                                      (if (eq? b b1) 'value b1))))))))

(actor (forever (on (message (set-cell $name $expr))
                    (define actor-expr (cell-expr->actor-expr name expr))
                    ;; (local-require racket/pretty) (pretty-print actor-expr)
                    (eval actor-expr (namespace-anchor->namespace ns)))))

(actor (forever (on (asserted (cell $name $value))
                    (printf ">>> ~a ~v\n" name value)
                    (flush-output))))

(actor (void (thread (lambda ()
                       (let loop ()
                         (define cell-name (read))
                         (if (eof-object? cell-name)
                             (send-ground-message 'quit)
                             (let ((new-expr (read)))
                               (send-ground-message (set-cell cell-name new-expr))
                               (loop)))))))
       (until (message 'quit #:meta-level 1)
              (on (message (set-cell $name $expr) #:meta-level 1)
                  (send! (set-cell name expr)))))
