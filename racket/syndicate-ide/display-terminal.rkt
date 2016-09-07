#lang racket/base
;; Implicitly provides a factory via display.rkt's `register-tty-backend!`.

(require racket/set)
(require racket/match)
(require (only-in racket/vector vector-copy))
(require (prefix-in ansi: ansi))
(require "display.rkt")
(require "diff.rkt")

(struct terminal (input ;; InputPort
                  output ;; OutputPort
                  [displayed-screen #:mutable] ;; Screen
                  [pending-screen #:mutable] ;; Screen
                  [utf-8-input? #:mutable] ;; Boolean
                  [displayed-title #:mutable] ;; (Option String)
                  [pending-title #:mutable] ;; (Option String)
                  )
  #:methods gen:tty
  [(define (tty-pending-screen t) (terminal-pending-screen t))
   (define (set-tty-pending-screen! t s) (set-terminal-pending-screen! t s))
   (define (tty-reset t) (reset t))
   (define (tty-flush t) (terminal-flush t))
   (define (tty-set-title! t title) (set-terminal-pending-title! t title))
   (define (tty-next-key t) (terminal-next-key t))
   (define (tty-next-key-evt t) (terminal-next-key-evt t))
   (define (tty-input-available-evt t) (terminal-input t))])

(define *stdin-tty* #f)
(define (stdin-tty)
  (when (not *stdin-tty*)
    (ansi:tty-raw!)
    (set! *stdin-tty*
          (terminal
           (current-input-port)
           (current-output-port)
           (make-screen 24 80 tty-default-pen)
           (make-screen 24 80 tty-default-pen)
           (match (getenv "RMACS_UTF8_INPUT")
             [(or #f "yes" "true" "1") #t]
             [(or "no" "false" "0") #f]
             [v (error 'RMACS_UTF8_INPUT
                       "Environment variable RMACS_UTF8_INPUT value ~v invalid: must be in ~v"
                       v
                       (list "yes" "true" "1" "no" "false" "0"))])
           #f
           #f))
    (reset *stdin-tty*)
    (plumber-add-flush! (current-plumber)
                        (lambda (h)
                          (output *stdin-tty*
                                  (ansi:select-graphic-rendition ansi:style-normal)
                                  (ansi:goto (tty-rows *stdin-tty*) 1))
                          (flush *stdin-tty*))))
  *stdin-tty*)

(define (collect-position-report tty)
  (let loop ()
    (sync/timeout 0.5
                  (handle-evt (terminal-input tty)
                              (lambda (p)
                                (match (ansi:lex-lcd-input p)
                                  [(? ansi:position-report? r) r]
                                  [_ (loop)]))))))

(define (reset tty)
  (output tty
          (ansi:clear-screen)
          (ansi:goto 999 999)
          (ansi:position-report-request))
  (flush tty)
  (define report (or (collect-position-report tty)
                     (ansi:position-report 24 80))) ;; TODO: have a more flexible fallback
  (define rows (ansi:position-report-row report))
  (define columns (ansi:position-report-column report))
  (set-pen tty tty-default-pen #:force #t)
  (clear tty)
  (flush tty)
  (set-terminal-displayed-screen! tty (make-screen rows columns tty-default-pen))
  (set-terminal-pending-screen! tty (make-screen rows columns tty-default-pen))
  tty)

(define (set-pen tty p #:force [force #f])
  (when (or force (not (equal? p (screen-pen (terminal-displayed-screen tty)))))
    (match p
      [(pen fgcolor bgcolor bold? italic?)
       (output tty
               (apply ansi:select-graphic-rendition
                      `(,@(if bold? (list ansi:style-bold) (list))
                        ,@(if italic? (list ansi:style-italic/inverse) (list))
                        ,(ansi:style-text-color fgcolor)
                        ,(ansi:style-background-color bgcolor))))]
      ['default
       (output tty (ansi:select-graphic-rendition ansi:style-normal))])
    (set-screen-pen! (terminal-displayed-screen tty) p))
  tty)

(define (clear tty)
  (output tty (ansi:clear-screen/home))
  (set-screen-cursor-row! (terminal-displayed-screen tty) 0)
  (set-screen-cursor-column! (terminal-displayed-screen tty) 0)
  tty)

(define (color-near-cursor s row-delta column-delta)
  (define r (max 0 (min (- (screen-rows s) 1) (+ (screen-cursor-row s) row-delta))))
  (define c (max 0 (min (- (screen-columns s) 1) (+ (screen-cursor-column s) column-delta))))
  (car (vector-ref (vector-ref (screen-contents s) r) c)))

(define (vector-delete! v base count fill)
  (vector-copy! v base v (+ base count) (vector-length v))
  (for ((i (in-range (- (vector-length v) count) (vector-length v)))) (vector-set! v i fill)))

(define (vector-insert! v base count fill)
  (vector-copy! v (+ base count) v base (- (vector-length v) count))
  (for ((i (in-range base (+ base count)))) (vector-set! v i fill)))

(define (delete-lines tty n)
  (define s (terminal-displayed-screen tty))
  (set-pen tty tty-default-pen)
  (output tty (ansi:delete-lines n))
  (define blank-line (make-vector (screen-columns s) (cons (screen-pen s) 'empty)))
  (vector-delete! (screen-contents s) (screen-cursor-row s) n blank-line)
  tty)

(define (insert-lines tty n)
  (define s (terminal-displayed-screen tty))
  (set-pen tty tty-default-pen)
  (output tty (ansi:insert-lines n))
  (define blank-line (make-vector (screen-columns s) (cons (screen-pen s) 'empty)))
  (vector-insert! (screen-contents s) (screen-cursor-row s) n blank-line)
  tty)

(define (delete-columns tty n)
  (define s (terminal-displayed-screen tty))
  (set-pen tty tty-default-pen)
  (output tty (ansi:delete-characters n))
  (define blank-cell (cons (screen-pen s) 'empty))
  (define line (vector-ref (screen-contents s) (screen-cursor-row s)))
  (vector-delete! line (screen-cursor-column s) n blank-cell)
  tty)

(define (insert-columns tty n)
  (define s (terminal-displayed-screen tty))
  (set-pen tty (color-near-cursor s 0 -1))
  (output tty (ansi:insert-characters n))
  (define blank-cell (cons (screen-pen s) 'empty))
  (define line (vector-ref (screen-contents s) (screen-cursor-row s)))
  (vector-insert! line (screen-cursor-column s) n blank-cell)
  tty)

(define (output tty . items)
  (for ((i items)) (display i (terminal-output tty))))

(define (flush tty)
  (flush-output (terminal-output tty)))

;;---------------------------------------------------------------------------
;; Display to buffered screen

(define (goto-if-needed s row column)
  (cond
   [(and (= (screen-cursor-row s) row) (= (screen-cursor-column s) column))
    ""]
   [(= (screen-cursor-row s) row)
    (begin0 (ansi:goto-column (+ column 1))
      (set-screen-cursor-column! s column))]
   [else
    (begin0 (ansi:goto (+ row 1) (+ column 1))
      (set-screen-cursor-row! s row)
      (set-screen-cursor-column! s column))]))

(define (advance-cursor! tty s)
  (set-screen-cursor-column! s (+ (screen-cursor-column s) 1))
  (when (= (screen-cursor-column s) (screen-columns s))
    (when (< (screen-cursor-row s) (- (screen-rows s) 1))
      (output tty "\r\n"))
    (set-screen-cursor-column! s 0)
    (set-screen-cursor-row! s (+ (screen-cursor-row s) 1))))

;; Answers #t when an edit to a line would produce a visible effect.
(define (interesting-change? old-line new-line column right-margin)
  (for/or [(i (in-range column right-margin))]
    (not (equal? (vector-ref old-line i) (vector-ref new-line i)))))

(define (non-empty? ch) (not (equal? ch 'empty)))

(define (repair-span! tty old new-line row first-col cell-count)
  (define trailing-empty-count
    (for/fold [(empty-count 0)] [(column (in-range first-col (+ first-col cell-count)))]
      (match-define (cons new-pen new-ch) (vector-ref new-line column))
      (if (non-empty? new-ch)
          (begin (set-pen tty new-pen)
                 (output tty (goto-if-needed old row column) new-ch)
                 (advance-cursor! tty old)
                 0)
          (+ empty-count 1))))
  (when (and (positive? trailing-empty-count) (= (+ first-col cell-count) (tty-columns tty)))
    (output tty (ansi:clear-to-eol))))

(define (repair-line! tty old new row)
  (define columns (screen-columns new))
  (define old-line (vector-ref (screen-contents old) row))
  (define new-line (vector-ref (screen-contents new) row))
  (define patches (diff-indices old-line new-line))
  (if (<= (length patches) 3)
      (apply-patch! patches
                    (lambda (first-col cols-to-remove)
                      (when (interesting-change? old-line new-line first-col columns)
                        (output tty (goto-if-needed old row first-col))
                        (delete-columns tty cols-to-remove)))
                    (lambda (first-col cols-to-insert cell-count)
                      (when (interesting-change? old-line new-line first-col columns)
                        (output tty (goto-if-needed old row first-col))
                        (when (and (positive? cols-to-insert)
                                   (interesting-change? old-line
                                                        new-line
                                                        (+ first-col cols-to-insert)
                                                        columns))
                          (insert-columns tty cols-to-insert))
                        (repair-span! tty old new-line row first-col cell-count))))
      (repair-span! tty old new-line row 0 columns)))

(define (terminal-flush t)
  (define old (terminal-displayed-screen t))
  (define new (terminal-pending-screen t))
  (apply-patch! (diff-indices (screen-contents old) (screen-contents new))
                (lambda (first-row lines-to-remove)
                  (output t (goto-if-needed old first-row (screen-cursor-column old)))
                  (delete-lines t lines-to-remove))
                (lambda (first-row lines-to-insert line-count)
                  (when (positive? lines-to-insert)
                    (output t (goto-if-needed old first-row (screen-cursor-column old)))
                    (insert-lines t lines-to-insert))
                  (for ((row (in-range first-row (+ first-row line-count))))
                    (repair-line! t old new row))))
  (output t (goto-if-needed old (screen-cursor-row new) (screen-cursor-column new)))
  (let ((new-title (terminal-pending-title t)))
    (when (not (equal? new-title (terminal-displayed-title t)))
      (when new-title (output t (ansi:xterm-set-window-title new-title)))
      (set-terminal-displayed-title! t new-title)))
  (flush t)
  (set-terminal-displayed-screen! t (struct-copy screen new [pen (screen-pen old)]))
  (set-terminal-pending-screen! t (copy-screen new))
  t)

;;---------------------------------------------------------------------------
;; Input

(define (has-control-modifier? modifiers)
  (set-member? modifiers 'control))

(define (terminal-next-key tty)
  (define k (ansi:lex-lcd-input (terminal-input tty) #:utf-8? (terminal-utf-8-input? tty)))
  (match k
    [(ansi:key #\tab modifiers) (ansi:key 'tab modifiers)]
    [(ansi:key #\I (? has-control-modifier? ms)) (ansi:key 'tab (set-remove ms 'control))]
    [(ansi:key #\M (? has-control-modifier? ms)) (ansi:key 'return (set-remove ms 'control))]
    [(ansi:key #\[ (? has-control-modifier? ms)) ;; ESC
     (or (sync/timeout 0.5
                       (handle-evt (terminal-next-key-evt tty)
                                   (lambda (k) (ansi:add-modifier 'meta k))))
         (ansi:key 'escape (set-remove ms 'control)))]
    [_ k]))

(define (terminal-next-key-evt tty)
  (handle-evt (terminal-input tty)
              (lambda (_) (terminal-next-key tty))))

(register-tty-backend! 'terminal stdin-tty)
