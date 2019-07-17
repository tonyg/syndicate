#lang syndicate

(require "../../widgets.rkt")
(require "cells-canvas.rkt")
(require racket/set racket/list racket/format)

;; a simple spreadsheet (will not check for circularities)

(require 7GUI/task-7-exp)
(require 7GUI/task-7-view)

;; -----------------------------------------------------------------------------
(struct formula (formula dependents) #:transparent)
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

(define (spawn-control frame)
  (spawn
   (field [*content  (make-immutable-hash)] ;; [Hashof Ref* Integer]
          [*formulas (make-immutable-hash)] ;; [Hashof Ref* Formula]
          )


(define-syntax-rule (iff selector e default) (let ([v e]) (if v (selector v) default)))
(define (get-exp ref*) (iff formula-formula (hash-ref (*formulas) ref* #f) 0))
(define (get-dep ref*) (iff formula-dependents (hash-ref (*formulas) ref* #f) (set)))
(define (get-content ref*) (hash-ref (*content) ref* 0))

(local-require 7GUI/should-be-racket)
(define (set-content! ref* vc)
  (define current (get-content ref*))
  (*content (hash-set (*content) ref* vc))
  (when (and current (not (= current vc)))
    (when* (get-dep ref*) => propagate-to)))

(define (propagate-to dependents)
  (for ((d (in-set dependents)))
    (set-content! d (evaluate (get-exp d) (*content)))))

(define (set-formula! ref* exp*)
  (define new     (formula exp* (or (get-dep ref*) (set))))
  (*formulas (hash-set (*formulas) ref* new))
  (register-with-dependents (depends-on exp*) ref*)
  (set-content! ref* (evaluate exp* (*content))))

(define (register-with-dependents dependents ref*)
  (for ((d (in-set dependents)))
    (*formulas (hash-set (*formulas) d (formula (get-exp d) (set-add (get-dep d) ref*))))))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents
(define ((mk-edit title-fmt validator registration source frame) x y)
  (define cell (list (x->A x) (y->0 y)))
  (when (and (first cell) (second cell))
    (react
     (define value0 (~a (or (source cell) "")))
     ;; maybe need to make use of queue-callback ?
     (define dialog (spawn-dialog #:parent #f
                                  #:style '(close-button)
                                  #:label (format title-fmt cell)))
     (define tf (spawn-text-field #:parent dialog
                                  #:label #f
                                  #:min-width 200
                                  #:min-height 80
                                  #:init-value value0))
     (on (message (text-field-enter tf $contents))
         (when* (validator contents)
                => (lambda (valid)
                     (stop-current-facet
                      (send! (show dialog #f))
                      (registration cell valid)
                      (send! (update-grid (*content)))))))
     (on (asserted (dialog@ dialog))
         (send! (show dialog #t))))))

(define content-edit (mk-edit "content for cell ~a" valid-content set-content! get-content frame))

(define formula-fmt "a formula for cell ~a")
(define formula-edit (mk-edit formula-fmt string->exp* set-formula! (compose exp*->string get-exp) frame))

;; ---------------------------------------------------------------------------------------------------
(on (message (single-click $x $y))
    (content-edit x y))
(on (message (double-click $x $y))
    (formula-edit x y))
(on-start (send! (update-grid (*content))))
))

;; ---------------------------------------------------------------------------------------------------
(spawn
(define frame  (spawn-frame #:label "Cells" #:width (/ WIDTH 2) #:height (/ HEIGHT 3)))
(define canvas (spawn-cells-canvas frame WIDTH HEIGHT))
(spawn-control frame)

(on (asserted (frame@ frame))
    (send! (show frame #t)))
)
