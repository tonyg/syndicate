#lang racket

(require "core.rkt")
(require "trie.rkt")
(require "pattern.rkt")
(require "upside-down.rkt")
(require (for-syntax racket/match))
(require (for-syntax syntax/parse))
(require automata/machine)
(require automata/re)

(provide trace-actor
         spawn-monitor
         assertion-added
         assertion-removed)

(module+ test
  (require rackunit))

#|
Provides a facility for defining and listening for event traces in upside-down
dataspaces. Trace actors listen for events (*exactly* the events described by
the input trace; perhaps they should generalize?) and update a state machine
accordingly, quitting the enclosing dataspace and invoking a callback if the
a complete trace is observed.
|#

; a Trace is a
; (trace TraceItem ...)

; a TraceItem is one of
; Pattern
; (seq TraceItem ...)
; (branch TraceItem ...)
; (loop TraceItem)
; anything

; a Pattern is one of
; (assertion-added P)
; (assertion-removed P)
; (message P)
; (and Pattern ...)
; (or Pattern ...)

; TraceItems are regular expressions where the constructors have the meaning you
; would expect. 'anything' is shorthand for (loop _). (This seems to mean that
; if 'anything' is included in a trace then the resulting actor listens for
; *all* events? No! only (assertion-added _) would cause that).
; One thing to note is that patterns are over the *upside-down* versions. So
; (assertion-added "hello") matches a patch where (upside-down "hello") is in
; the added trie.

(begin-for-syntax
  ;; pattern-syntax -> syntax
  ;; produces syntax describing an assertion set (trie) containing a
  ;; subscription to the input syntax
  (define (pattern->subscription P)
    (match-define-values (_ assertion-set-pattern _ _) (analyze-pattern P P))
    #`(pattern->trie '<trace-pattern>
                     (observe (upside-down #,assertion-set-pattern))))
  
  (define-syntax-class trace-pattern
    ;; subscription is a trie containing a subscription to assertions relevant
    ;; to the pattern match-pattern is a match pattern for events matching the
    ;; pattern
    #:attributes (subscription match-pattern)
    #:datum-literals (assertion-added assertion-removed message and or)
    (pattern (assertion-added P:expr)
             #:attr match-pattern #'(assertion-added (upside-down P))
             #:attr subscription (pattern->subscription #'P))
    (pattern (assertion-removed P:expr)
             #:attr match-pattern #'(assertion-removed (upside-down P))
             #:attr subscription (pattern->subscription #'P))
    (pattern (message P:expr)
             #:attr match-pattern #'(message (upside-down P))
             #:attr subscription (pattern->subscription #'P))
    (pattern (or pat:trace-pattern ...)
             #:attr match-pattern #'(or pat.match-pattern ...)
             #:attr subscription #'(trie-union-all (list pat.subscription ...)))
    (pattern (and pat:trace-pattern ...)
             #:attr match-pattern #'(and pat.match-pattern ...)
             #:attr subscription #'(trie-union-all (list pat.subscription ...))))
  
  (define-syntax-class trace-item
    ;; subscription is a trie containing a subscription to assertions relevant
    ;; to the pattern
    ;; re is a regular expression describing the item
    #:attributes (subscription re)
    #:datum-literals (seq branch loop anything)
    (pattern (seq t:trace-item ...)
             #:attr subscription #'(trie-union-all (list t.subscription ...))
             #:attr re #'(seq t.re ...))
    (pattern (branch t:trace-item ...)
             #:attr subscription #'(trie-union-all (list t.subscription ...))
             #:attr re #'(union t.re ...))
    (pattern (loop t:trace-item)
             #:attr subscription #'t.subscription
             #:attr re #'(star t.re))
    (pattern p:trace-pattern
             #:attr subscription #'p.subscription
             #:attr re #'p.match-pattern)
    (pattern anything
             #:attr subscription #'trie-empty
             #:attr re #'(star _)))

  (define-syntax-class trace
    #:attributes (subscription re)
    #:datum-literals (trace)
    (pattern (trace t:trace-item ...)
             #:attr subscription #'(trie-union-all (list t.subscription ...)
                                                   #:combiner (lambda (v1 v2) (trie-success v1)))
             #:attr re #'(re (seq t.re ...)))))

;; Machine Patch (Bool -> ) -> Actor
(define-syntax (trace-actor stx)
  (syntax-parse stx
    [(_ t:trace f:expr)
     #'(spawn-monitor t.re
                      (patch t.subscription
                             trie-empty)
                      f)]))

;; Machine Patch ( -> ) -> Actor
;; Create an actor with the given interests
;; Feed incoming events to the given machine
;; If the machine accepts, call the given thunk then issue a quit-dataspace
;; action
(define (spawn-monitor m interests f)
  (actor (lambda (e m)
           (and e
                (let ([next-m (m e)])
                  (cond
                    [(machine-accepting? next-m)
                     (f)
                     (quit (quit-dataspace))]
                    [else
                     (transition next-m '())]))))
         m
         interests))

(define (pattern-in-trie? t p)
  (not (trie-empty? (trie-project t p))))

(define-match-expander assertion-added
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat)
       (let ()
         (match-define-values (_ assertion-set-pattern _ _)
                              (analyze-pattern stx #'pat))
         (with-syntax [(trie-pat assertion-set-pattern)]
           #'(and (? patch?)
                  (? (lambda (p) (pattern-in-trie? (patch-added p)
                                                   trie-pat))))))])))

(define-match-expander assertion-removed
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat)
       (let ()
         (match-define-values (_ assertion-set-pattern _ _)
                              (analyze-pattern stx #'pat))
         (with-syntax [(trie-pat assertion-set-pattern)]
           #'(and (? patch?)
                  (? (lambda (p) (pattern-in-trie? (patch-removed p)
                                                   trie-pat))))))])))

(module+ test
  (check-equal? (match (patch (pattern->trie 'x 123) trie-empty)
                  [(assertion-added 123)
                   'wippee]
                  [_ 'boo])
                'wippee)
  (check-equal? (match (patch (pattern->trie 'x 123)
                              (pattern->trie 'y 'good-bye))
                  [(assertion-removed 123)
                   'fudge]
                  [(assertion-removed 'good-bye)
                   'hellz-yeah]
                  [_ 'boo])
                'hellz-yeah)
  (let ()
    (struct test-struct (x y) #:transparent)
    (check-equal? (match (patch (pattern->trie 'x (test-struct 5 6)) trie-empty)
                    [(assertion-added (test-struct 5 _))
                     'good]
                    [_ 'bad])
                  'good))
  (check-equal? (match (patch (pattern->trie 'x 5) (pattern->trie 'x 6))
                  [(and (assertion-added 5) (assertion-removed 6))
                   #t]
                  [_ #f])
                #t))