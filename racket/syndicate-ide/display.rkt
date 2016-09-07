#lang racket/base

(provide gen:tty
         tty?
         tty-pending-screen
         set-tty-pending-screen!
         tty-rows
         tty-columns
         tty-last-row
         tty-last-column
         tty-cursor-row
         tty-cursor-column
         tty-display
         tty-newline
         tty-clear
         tty-clear-to-eol
         tty-reset
         tty-goto
         tty-set-pen!
         tty-default-pen
         tty-pen
         tty-flush
         tty-set-title!
         tty-next-key
         tty-next-key-evt
         tty-input-available-evt

         (struct-out pen)

         register-tty-backend!
         default-tty

         (struct-out screen)
         make-screen
         copy-screen
         screen-last-row
         screen-last-column
         screen-goto
         screen-putc
         screen-puts
         screen-clear-to-eol

         ;; From ansi. TODO: better color & keyboard abstractions
         (struct-out key)
         (rename-out [ansi:color-black color-black]
                     [ansi:color-red color-red]
                     [ansi:color-green color-green]
                     [ansi:color-yellow color-yellow]
                     [ansi:color-blue color-blue]
                     [ansi:color-magenta color-magenta]
                     [ansi:color-cyan color-cyan]
                     [ansi:color-white color-white]))

(require racket/match)
(require racket/generic)
(require (only-in racket/vector vector-copy))
(require (prefix-in ansi: (only-in ansi
                                   color-black
                                   color-red
                                   color-green
                                   color-yellow
                                   color-blue
                                   color-magenta
                                   color-cyan
                                   color-white)))
(require (only-in ansi struct:key key key? key-value key-modifiers))

;; A Color is a Nat. TODO: better color abstraction.

(define-generics tty
  (tty-pending-screen tty)
  (set-tty-pending-screen! tty s)
  (tty-reset tty)
  (tty-flush tty)
  (tty-set-title! tty title)
  (tty-next-key tty)

  ;; Do not retain the events returned by these functions across
  ;; actual input from the tty! See comment in editor-sit-for, and
  ;; implementations of these functions in display-gui.rkt. (The
  ;; fragility of the pushback in display-gui.rkt is the cause of this
  ;; restriction.)
  (tty-next-key-evt tty)
  (tty-input-available-evt tty)
  )

(define (tty-rows t) (screen-rows (tty-pending-screen t)))
(define (tty-columns t) (screen-columns (tty-pending-screen t)))

(define (tty-last-row t) (- (tty-rows t) 1))
(define (tty-last-column t) (- (tty-columns t) 1))

(define (tty-cursor-row t) (screen-cursor-row (tty-pending-screen t)))
(define (tty-cursor-column t) (screen-cursor-column (tty-pending-screen t)))

(define (tty-display t . strings)
  (for [(str strings)]
    (screen-puts (tty-pending-screen t) str)))

(define (tty-newline t)
  (define s (tty-pending-screen t))
  (screen-clear-to-eol s)
  (screen-putc s #\return)
  (screen-putc s #\newline))

(define (tty-clear t)
  (define s (tty-pending-screen t))
  (set-tty-pending-screen! t (make-screen (screen-rows s) (screen-columns s) (screen-pen s)))
  t)

(define (tty-clear-to-eol t) (screen-clear-to-eol (tty-pending-screen t)))
(define (tty-goto t row col) (screen-goto (tty-pending-screen t) row col))
(define (tty-set-pen! t p) (set-screen-pen! (tty-pending-screen t) p))
(define (tty-pen t) (screen-pen (tty-pending-screen t)))

(define tty-default-pen 'default)

(struct pen (foreground-color ;; Color
             background-color ;; Color
             bold? ;; Boolean
             italic? ;; Boolean
             ) #:prefab)

(struct backend (name ;; Symbol
                 priority ;; Integer
                 factory ;; (-> TTY)
                 )
  #:prefab)

(struct screen (rows ;; Nat
                columns ;; Nat
                [cursor-row #:mutable] ;; Nat
                [cursor-column #:mutable] ;; Nat
                [pen #:mutable] ;; Pen
                contents ;; (Vector[rows] (Vector[columns] (Cons Pen Character)))
                ) #:prefab)

(define (screen-last-row s) (- (screen-rows s) 1))
(define (screen-last-column s) (- (screen-columns s) 1))

(define (make-screen rows columns pen)
  (define contents (for/vector ((row rows)) (make-vector columns (cons pen 'empty))))
  (screen rows columns 0 0 pen contents))

(define (copy-screen s)
  (match-define (screen rows columns cursor-row cursor-column pen contents) s)
  (define new-contents (for/vector ((row rows)) (vector-copy (vector-ref contents row))))
  (screen rows columns cursor-row cursor-column pen new-contents))

(define (screen-goto s row0 column0)
  (define row (max 0 (min (screen-last-row s) row0)))
  (define column (max 0 (min (screen-last-column s) column0)))
  (set-screen-cursor-row! s row)
  (set-screen-cursor-column! s column)
  s)

(define (non-empty? ch) (not (equal? ch 'empty)))

(define (screen-putc s ch)
  (match ch
    [#\return
     (screen-goto s (screen-cursor-row s) 0)]
    [#\newline
     (screen-goto s (+ (screen-cursor-row s) 1) (screen-cursor-column s))]
    [#\tab
     (for ((i (- 8 (modulo (screen-cursor-column s) 8)))) (screen-putc s #\space))]
    [(and (? non-empty?) (? char-iso-control?))
     (screen-puts s (format "[~x]" (char->integer ch)))]
    [_
     (when (< (screen-cursor-column s) (screen-columns s))
       (vector-set! (vector-ref (screen-contents s) (screen-cursor-row s))
                    (screen-cursor-column s)
                    (cons (screen-pen s) ch)))
     (set-screen-cursor-column! s (+ (screen-cursor-column s) 1))]))

(define (screen-puts s str)
  (for ((ch str)) (screen-putc s ch)))

(define (screen-clear-to-eol s)
  (define start-column (screen-cursor-column s))
  (define pen (screen-pen s))
  (set-screen-pen! s tty-default-pen)
  (for ((i (max 0 (- (screen-columns s) (screen-cursor-column s)))))
    (screen-putc s 'empty))
  (set-screen-pen! s pen)
  (screen-goto s (screen-cursor-row s) start-column)
  s)

(define *tty-backends* '())

(define (register-tty-backend! name factory #:priority [priority 0])
  (set! *tty-backends* (cons (backend name priority factory)
                             (filter (lambda (b) (not (eq? (backend-name b) name)))
                                     *tty-backends*)))
  (set! *tty-backends* (sort *tty-backends* > #:key backend-priority)))

(define *default-tty* #f)
(define (default-tty)
  (when (not *default-tty*)
    (let loop ((backends *tty-backends*))
      (match backends
        ['() (error 'default-tty "No available tty backends")]
        [(cons (backend name _priority factory) rest)
         (define t (factory))
         (if t
             (set! *default-tty* t)
             (loop rest))])))
  *default-tty*)
