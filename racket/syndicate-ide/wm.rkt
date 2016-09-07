#lang racket/base
;; TUI Window Manager

;; A TBox is a thing that can be laid out, displayed, and interacted
;; with.

;; TBoxes can be placed in relationship to other boxes:
;;  - h{t,c,b}-append
;;  - v{l,c,r}-append
;;  - {l,c,r}{t,c,b}-superimpose
;;  - wrap

;; Sources of inspiration:
;;   http://icie.cs.byu.edu/cs456/UIBook/05-Layout.pdf
;;   http://doc.qt.io/qt-5/qtwidgets-tutorials-widgets-nestedlayouts-example.html
;;   http://www.math.utah.edu/~beebe/reports/2009/boxes.pdf

;; EXAMPLES for developing intuition:
;;  1. a button
;;  2. a scrollbar
;;  3. a list of items with a panel to its right having a
;;     variable-width pretty-printing of the selected item
;;
;; Button: Minimum size reflects minimal chrome; simply the smallest
;; possible workable size. Desired and maximum size are the same,
;; large enough to contain the usual chrome.
;;
;; Scrollbar: wolog, horizontal. Height is fixed. Minimum width
;; reflects thumbless condition. Desired width might reflect some
;; arbitrary size where the thumb could be moved around. Max width
;; would usually involve a horizontal fill of some weight and rank.
;; Hmm, but then why not have the desired width just be the max width?
;; Perhaps desired and max are the same?
;;
;; Items and pretty-printing: Something like this:
;;
;; +----------+------------+
;; |*(foo ...*| (foo (bar  |
;; | (b () .. |       zot) |
;; | 123      |      ()    |
;; |          |      quux  |
;; |          |      baz)  |
;; +----------+------------+
;;
;; We want the item list to get some reasonable minimal amount of
;; space, and otherwise to take up space not used by the
;; pretty-printing. The pretty-printing should try to use vertical
;; space within reason and otherwise should try to be as compact as
;; possible.
;;
;; ---
;;
;; This min/desired/max split is a bit clunky. Could we have a list of
;; preferred TeX-style sizings, ordered most-preferred first? They
;; could include information to send back to the box at render time.
;; For example, the button might offer horizontal sizings
;;
;; (list (sizing 'normal-chrome 10 (fill 1 1) 2)
;;       (sizing 'no-chrome 6 0 0))
;;
;; ---
;;
;; How does arithmetic on sizings work?
;;
;; Ideals are never fills, they're simply naturals. They can be
;; added/min'd/max'd as usual.
;;
;; Stretch is sometimes a natural, and sometimes a fill.
;;
;;    n         + fill w  r = fill w r
;;    fill w1 r + fill w2 r = fill (w1 + w2) r
;;    fill _  s + fill w  r = fill w r          when r > s
;;
;; The definitions of `max` is similar, with `max` for `+`. A fill
;; behaves as a zero for the purposes of `min`.

(require racket/generic)

;;---------------------------------------------------------------------------

;; A Fill is one of
;; - a Nat, a fixed amount of space
;; - a (fill Nat Nat), a potentially infinite amount of space
(struct fill (weight rank) #:transparent)

;; A Sizing is a (sizing Nat Fill Fill)
(struct sizing (ideal stretch shrink) #:transparent)

;; (Nat Nat -> Nat) -> (Fill Fill -> Fill)
(define ((fill-binop op) a b)
  (match* (a b)
    [((? number?) (? number?)) (op a b)]
    [((? number?) (? fill?)) b]
    [((? fill?) (? number?)) a]
    [((fill w1 r1) (fill w2 r2))
     (cond [(= r1 r2) (fill (op w1 w2) r1)]
           [(> r1 r2) (fill w1 r1)]
           [(< r1 r2) (fill w2 r2)])]))

;; Fill Fill -> Fill
(define fill+ (fill-binop +))
(define fill-max (fill-binop max))
(define (fill-min a b)
  (if (and (number? a) (number? b))
      (min a b)
      0))

;; (Nat Nat -> Nat) (Fill Fill -> Fill) -> (Sizing Sizing -> Sizing)
;;
;; TODO: for max and min, do we really want fop to be used for both
;; stretch and shrink?
;;
(define ((sizing-binop iop fop) a b)
  (match-define (sizing ia ta ha) a)
  (match-define (sizing ib tb hb) b)
  (sizing (iop ia ib) (fop ta tb) (fop ha hb)))

;; Sizing Sizing -> Sizing
(define sizing+ (sizing-binop + fill+))
(define sizing-max (sizing-binop max fill-max))
(define sizing-min (sizing-binop min fill-min))

;;---------------------------------------------------------------------------

(define-generics tbox
  ;; TBox (Option Nat) (Option Nat) -> (Listof (List Sizing Sizing))
  (tbox-sizings tbox maybe-speculative-width maybe-speculative-height)
  ;; TBox TTY Nat Nat Nat Nat -> Void
  (tbox-render! tbox tty top left width height))

(struct glue-tbox (horizontal vertical string pen) #:transparent
  #:methods gen:tbox
  [(define (tbox-sizings t w h)
     (list (list (glue-tbox-horizontal t) (glue-tbox-vertical t))))
   (define (tbox-render! t tty top left width height)
     (define str (fill-tbox-string t))
     (define whole-repeats (quotient width (string-length str)))
     (define fragment (substring str 0 (remainder width (string-length str))))
     (tty-set-pen! tty (fill-tbox-pen t))
     (for [(y (in-range height))]
       (tty-goto tty (+ top y) left)
       (for [(i (in-range whole-repeats))] (tty-display tty str))
       (tty-display tty fragment)))])

;; Nat -> (Cons X (Listof X)) -> X
(define ((nth-or-last n) xs)
  (cond [(zero? n) (car xs)]
        [(null? (cdr xs)) (car xs)]
        [else (drop-n-or-last (- n 1) (cdr xs))]))

(define (layout-horizontal items width height)
  (define item-count (length items))
  (define size-preferences (map (lambda (i) (tbox-sizings i #f #f)) items))
  (let try-nth-choices ((nth-choice 0))
    (define candidates (map (nth-or-last nth-choice) size-preferences))
    (define aggregate-h (apply sizing+ (map car candidates)))
    ...
    
