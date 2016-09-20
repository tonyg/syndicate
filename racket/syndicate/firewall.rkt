#lang racket/base
;; "Firewall" communications from a process.

(provide spawn-firewall
         (rename-out [firewall-actor firewall]))

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/srcloc))

(require racket/match)
(require (only-in racket/list filter-map))
(require "core.rkt")
(require "trie.rkt")
(require "pretty.rkt")
(require (only-in "actor.rkt"
                  react
                  actor-action
                  syndicate-effects-available?
                  schedule-action!
                  actor-name))

(struct firewall (limit ;; AssertionSet
                  inner ;; Process
                  )
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print f [p (current-output-port)])
     (pretty-print-firewall f p))])

(define (pretty-print-firewall f p)
  (fprintf p "FIREWALL\n")
  (syndicate-pretty-print (process-state (firewall-inner f)) p))

(define-syntax (firewall-actor stx)
  (syntax-parse stx
    [(_ [patch-expr ...] name:actor-name O ...)
     (quasisyntax/loc stx
       (let* ((inner-action (actor-action #:name name.N (react O ...)))
              (limit (apply-patch trie-empty (patch-seq (interpret-patch-expr patch-expr) ...)))
              (spawn-action (spawn-firewall limit inner-action)))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (interpret-patch-expr stx)
  (syntax-parse stx
    [(_ ((~literal allow) expr)) (syntax/loc stx (assert expr))]
    [(_ ((~literal forbid) expr)) (syntax/loc stx (retract expr))]))

(define (spawn-firewall limit inner-spawn)
  (make-spawn (lambda ()
                (define-values (proc initial-transition) (spawn->process+transition inner-spawn))
                (list firewall-handle-event
                      (firewall-transition initial-transition (firewall limit proc))
                      (process-name proc)))))

(define (firewall-transition t f)
  (match t
    [(<quit> exn actions)
     (<quit> exn (firewall-actions actions (firewall-limit f)))]
    [(transition st actions)
     (transition (struct-copy firewall f [inner (update-process-state (firewall-inner f) st)])
                 (firewall-actions actions (firewall-limit f)))]
    [(or #f (? void?))
     t]))

(define (firewall-actions acs limit)
  (filter-map (lambda (ac) (firewall-action ac limit)) (clean-actions acs)))

(define (limit-trie limit trie)
  (trie-intersect trie limit #:combiner (lambda (v1 v2) (trie-success v1))))

(define (firewall-action ac limit)
  (match ac
    [#f #f]
    [(message c)
     (and (trie-lookup limit c #f) ;; todo: handle wildcard as a value
          (message c))]
    [(patch a d)
     (patch (limit-trie limit a) (limit-trie limit d))]
    [(? spawn? s)
     (spawn-firewall limit s)]
    [_
     (error 'firewall-action "Cannot filter action ~v" ac)]))

(define (firewall-handle-event e f)
  (define i (firewall-inner f))
  (firewall-transition ((process-behavior i) e (process-state i)) f))
