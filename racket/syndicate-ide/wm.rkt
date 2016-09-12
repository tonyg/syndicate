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
;;
;; This is very useful:
;;   http://tex.stackexchange.com/questions/64756/what-is-glue-stretching

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
;; For example, the button might offer sizings
;;
;; (list (layout-option 'normal-chrome (sizing 10 (fill 1 1) 2) (sizing ...))
;;       (layout-option 'no-chrome (sizing 6 0 0) (sizing ...)))
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
(require racket/match)
(require racket/pretty)
(require (only-in racket/list flatten))
(require (only-in racket/string string-split))
(require (prefix-in ansi: ansi))

(require "display.rkt")

;;---------------------------------------------------------------------------

;; A Fill is one of
;; - a Nat, a fixed amount of space
;; - a (fill Nat Nat), a potentially infinite amount of space
(struct fill (weight rank) #:transparent)

;; A very weak fill.
(define weak-fill (fill 1 -1))

;; A Sizing is a (sizing Nat Fill Fill)
(struct sizing (ideal stretch shrink) #:transparent)

;; A LayoutOption is a (layout-option Any Sizing Sizing)
(struct layout-option (info horizontal-sizing vertical-sizing) #:transparent)

;; A Layout is a (layout Any Nat Nat Nat Nat)
(struct layout (info top left width height) #:transparent)

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

;;---------------------------------------------------------------------------

(define-generics tbox
  ;; TBox Sizing Sizing -> (Listof LayoutOption)
  (tbox-layout-options tbox h-sizing v-sizing)
  ;; TBox Any Nat Nat Nat Nat -> Layout
  (tbox-layout tbox info top left width height)
  ;; TBox TTY Layout -> Void
  (tbox-render! tbox tty layout)
  #:fallbacks [(define (tbox-layout-options t sw sh)
                 (list (layout-option #f
                                      (sizing (sizing-ideal sw) 0 0)
                                      (sizing (sizing-ideal sh) 0 0))))
               (define (tbox-layout tbox info top left width height)
                 (layout info top left width height))])

(struct glue-tbox (horizontal vertical string pen) #:transparent
  #:methods gen:tbox
  [(define (tbox-layout-options t w h)
     (list (layout-option #f (glue-tbox-horizontal t) (glue-tbox-vertical t))))
   (define (tbox-render! t tty lo)
     (match-define (layout _ top left width height) lo)
     (define str (glue-tbox-string t))
     (define whole-repeats (quotient width (string-length str)))
     (define fragment (substring str 0 (remainder width (string-length str))))
     (tty-set-pen! tty (glue-tbox-pen t))
     (for [(y (in-range height))]
       (tty-goto tty (+ top y) left)
       (for [(i (in-range whole-repeats))] (tty-display tty str))
       (tty-display tty fragment)))])

;; Nat -> (Cons X (Listof X)) -> X
(define ((nth-or-last n) xs)
  (let loop ((n n) (xs xs))
    (cond [(zero? n) (car xs)]
          [(null? (cdr xs)) (car xs)]
          [else (loop (- n 1) (cdr xs))])))

(define (transverse-bound sizings sizing-accessor minus-or-plus max-or-min)
  (define vals (for/list [(s sizings) #:when (number? (sizing-accessor s))]
                 (minus-or-plus (sizing-ideal s) (sizing-accessor s))))
  (values (and (pair? vals) (apply max-or-min vals))
          (foldl fill-max 0 (filter fill? (map sizing-accessor sizings)))))

(define (transverse-sizing sizings sv)
  (match-define (sizing v _ _) sv)
  (define-values (lb-v lb-f) (transverse-bound sizings sizing-shrink - max))
  (define-values (ub-v ub-f) (transverse-bound sizings sizing-stretch + min))
  (define ideal-v (foldl max 0 (map sizing-ideal sizings)))
  ;; (define ideal-v (if v
  ;;                     (cond [(and lb-v (> lb-v v)) lb-v]
  ;;                           [(and ub-v (< ub-v v)) ub-v]
  ;;                           [else v])
  ;;                     (or lb-v 0)))
  (sizing ideal-v
          (if ub-v (- ub-v ideal-v) ub-f)
          (if lb-v (- ideal-v lb-v) lb-f)))

(define (parallel-sizing sizings)
  (sizing (foldl + 0 (map sizing-ideal sizings))
          (foldl fill+ 0 (map sizing-stretch sizings))
          (foldl fill+ 0 (map sizing-shrink sizings))))

(define (sizing-contains? s v)
  (match-define (sizing x x+ x-) s)
  (cond [(>= v x) (if (number? x+) (<= v (+ x x+)) #t)]
        [(<= v x) (if (number? x-) (>= v (- x x-)) #t)]))

(define (sizing-min s)
  (match (sizing-shrink s)
    [(? number? n) (- (sizing-ideal s) n)]
    [(? fill?) -inf.0]))

(define (sizing-max s)
  (match (sizing-stretch s)
    [(? number? n) (+ (sizing-ideal s) n)]
    [(? fill?) +inf.0]))

(define (sizing-overlap? x y)
  ;; |-----|
  ;;    |-----|
  ;;
  ;; |--------|
  ;;    |--|
  ;;
  ;; |--|
  ;;       |--|
  ;;
  (define largest-min (max (sizing-min x) (sizing-min y)))
  (define smallest-max (min (sizing-max x) (sizing-max y)))
  (< largest-min smallest-max))

(define (fill-scale f scale)
  (if (number? f)
      (* f scale)
      f))

(define (sizing-scale s scale)
  (match-define (sizing x x+ x-) s)
  (sizing (* x scale) (fill-scale x+ scale) (fill-scale x- scale)))

(define ((acceptable-choice? width height) candidate)
  (match-define (layout-option _info w h) candidate)
  (and (sizing-overlap? w width)
       (sizing-overlap? h height)))

(define (adjacent-layout-options vertical? items sw sh)
  (define item-count (length items))
  (define fair-width (if (zero? item-count) sw (sizing-scale sw (/ item-count))))
  (define fair-height (if (zero? item-count) sh (sizing-scale sh (/ item-count))))
  (define size-preferences (map (if vertical?
                                    (lambda (i) (tbox-layout-options i sw fair-height))
                                    (lambda (i) (tbox-layout-options i fair-width sh)))
                                items))
  (define prefs-depth (apply max (map length size-preferences)))
  (define choices
    (for/list [(nth-choice (in-range prefs-depth))]
      (define candidates (map (nth-or-last nth-choice) size-preferences))
      (if vertical?
          (layout-option candidates
                         (transverse-sizing (map layout-option-horizontal-sizing candidates) sw)
                         (parallel-sizing (map layout-option-vertical-sizing candidates)))
          (layout-option candidates
                         (parallel-sizing (map layout-option-horizontal-sizing candidates))
                         (transverse-sizing (map layout-option-vertical-sizing candidates) sh)))))
  (define acceptable-choices (filter (acceptable-choice? sw sh) choices))
  (if (null? acceptable-choices)
      choices
      acceptable-choices))

(define (compute-concrete-adjacent-sizes sizings actual-bound)
  (define ideal-total (foldl + 0 (map sizing-ideal sizings)))
  (define-values (available-slop sizing-give apply-give)
    (if (<= ideal-total actual-bound)
        (values (- actual-bound ideal-total) sizing-stretch +)
        (values (- ideal-total actual-bound) sizing-shrink -)))
  (define total-give (foldl fill+ 0 (map sizing-give sizings)))
  (if (number? total-give)
      (let ((scale (if (zero? total-give) 0 (/ available-slop total-give))))
        (map (lambda (s)
               ;; numeric total-give ⇒ no fills for any give in the list
               (apply-give (sizing-ideal s) (* (sizing-give s) scale)))
             sizings))
      (let* ((weight (fill-weight total-give))
             (rank (fill-rank total-give))
             (scale (if (zero? weight) 0 (/ available-slop weight))))
        (map (lambda (s)
               (match (sizing-give s)
                 [(fill w (== rank)) (apply-give (sizing-ideal s) (* w scale))]
                 [_ (sizing-ideal s)]))
             sizings))))

(define (compute-concrete-adjacent-layout vertical? items candidates top left width height)
  (define actual-sizes
    (if vertical?
        (compute-concrete-adjacent-sizes (map layout-option-vertical-sizing candidates) height)
        (compute-concrete-adjacent-sizes (map layout-option-horizontal-sizing candidates) width)))
  (define-values (_last-pos entries-rev)
    (for/fold [(pos (if vertical? top left)) (entries-rev '())]
              [(entry candidates) (item items) (actual-size actual-sizes)]
      (define rpos (round pos))
      (define size (- (round (+ pos actual-size)) rpos))
      (values (+ pos actual-size)
              (cons (if vertical?
                        (tbox-layout item (layout-option-info entry) rpos left width size)
                        (tbox-layout item (layout-option-info entry) top rpos size height))
                    entries-rev))))
  (reverse entries-rev))

(struct adjacent-tbox (vertical? items) #:transparent
  #:methods gen:tbox
  [(define/generic render! tbox-render!)
   (define (tbox-layout-options t w h)
     (adjacent-layout-options (adjacent-tbox-vertical? t)
                              (adjacent-tbox-items t)
                              w
                              h))
   (define (tbox-layout t candidates top left width height)
     (layout (compute-concrete-adjacent-layout (adjacent-tbox-vertical? t)
                                               (adjacent-tbox-items t)
                                               candidates
                                               top
                                               left
                                               width
                                               height)
             top left width height))
   (define (tbox-render! t tty lo)
     (for [(item-lo (layout-info lo)) (item (adjacent-tbox-items t))]
       (render! item tty item-lo)))])

(define (fill-weaken f w r)
  (if (fill? f)
      (fill w r)
      f))

(define (sizing-weaken-fills s
                             [stretch-weight 1]
                             [stretch-rank 0]
                             [shrink-weight stretch-weight]
                             [shrink-rank stretch-rank])
  (match-define (sizing x x+ x-) s)
  (sizing x
          (fill-weaken x+ stretch-weight stretch-rank)
          (fill-weaken x- shrink-weight shrink-rank)))

(struct weaken-fills (item weight rank) #:transparent
  #:methods gen:tbox
  [(define/generic layout-options tbox-layout-options)
   (define/generic inner-layout tbox-layout)
   (define/generic render! tbox-render!)
   (define (tbox-layout-options t sw sh)
     (map (match-lambda
            [(layout-option info w h)
             (layout-option info
                            (sizing-weaken-fills w (weaken-fills-weight t) (weaken-fills-rank t))
                            (sizing-weaken-fills h (weaken-fills-weight t) (weaken-fills-rank t)))])
          (layout-options (weaken-fills-item t) sw sh)))
   (define (tbox-layout t info top left width height)
     (inner-layout (weaken-fills-item t) info top left width height))
   (define (tbox-render! t tty lo)
     (render! (weaken-fills-item t) tty lo))])

(define (sizing-adjust-ideal s i)
  (match-define (sizing x x+ x-) s)
  (sizing i
          (if (fill? x+) x+ (+ x+ (- x i)))
          (if (fill? x-) x- (- x- (- x i)))))

(struct pretty-print-tbox (value mode) #:transparent
  #:methods gen:tbox
  [(define (tbox-layout-options t sw sh)
     (define width-guess (sizing-ideal sw))
     (define height-guess (length (pretty-print-tbox-lines t (floor width-guess))))
     (list (layout-option #f sw (sizing-adjust-ideal sh height-guess))))
   (define (tbox-render! t tty lo)
     (tty-goto tty (layout-top lo) (layout-left lo))
     (for [(line (pretty-print-tbox-lines t (floor (layout-width lo))))]
       (tty-display tty line)
       (tty-goto tty (+ (tty-cursor-row tty) 1) (layout-left lo))))])

(define (pretty-print-tbox-contents t width)
  (pretty-format (pretty-print-tbox-value t)
                 width
                 #:mode (pretty-print-tbox-mode t)))

(define (pretty-print-tbox-lines t width)
  (reverse
   (for/fold [(lines '())] [(line (string-split (pretty-print-tbox-contents t width) "\n"))]
     (let loop ((lines lines) (line line))
       (if (> (string-length line) width)
           (loop (cons (substring line 0 width) lines) (substring line width))
           (cons line lines))))))

;;---------------------------------------------------------------------------

(define (fill* w h rank)
  (glue-tbox (sizing w (fill 1 rank) 0)
             (sizing h (fill 1 rank) 0)
             " "
             'default))

(define (hfil [w 0]) (fill* w 0 0))
(define (hfill [w 0]) (fill* w 0 1))
(define (hfilll [w 0]) (fill* w 0 2))

(define (vfil [h 0]) (fill* 0 h 0))
(define (vfill [h 0]) (fill* 0 h 1))
(define (vfilll [h 0]) (fill* 0 h 2))

(define (hbox . items) (adjacent-tbox #f (flatten items)))
(define (vbox . items) (adjacent-tbox #t (flatten items)))

(define (hpad item) (hbox (hfill) item (hfill)))
(define (vpad item) (vbox (vfill) item (vfill)))
(define (pad item) (vpad (hpad item)))

(define (weaken item [weight 1] [rank 0])
  (weaken-fills item weight rank))

;;---------------------------------------------------------------------------

(module+ main
  (require racket/pretty)
  (require racket/set)
  (require "display-terminal.rkt")

  (define tty (default-tty))

  (with-handlers [(values
                   (lambda (e)
                     (tty-shutdown!! tty)
                     (raise e)))]
    (tty-display tty "Ho ho ho\r\n")

    (define (colored-box bgcolor)
      (glue-tbox (sizing 10 (fill 1 0) 0)
                 (sizing 5 0 0)
                 ":"
                 (pen color-white bgcolor #f #f)))

    (define R (colored-box color-red))
    (define G (colored-box color-green))
    (define B (colored-box color-blue))

    (define (tbox-render-toplevel! widget tty)
      (define w (tty-columns tty))
      (define h (tty-rows tty))
      (define layouts (tbox-layout-options widget
                                           (sizing w 0 w)
                                           (sizing h 0 h)))
      (define lo (tbox-layout widget (layout-option-info (car layouts)) 0 0 w h))
      (tbox-render! widget tty lo))

    (define xpad values)

    (define toplevel-widget
      (hbox (weaken (vbox (xpad R)
                          (pad G)
                          (xpad B)))
            ;; (hfill)
            (vbox (weaken (hpad R)) (vfil))
            ;; (hfill)
            (vbox (vfil) G)
            ;; (hfill)
            (vbox (weaken (pad B))
                  (pretty-print-tbox
                   `(define (tbox-render-toplevel! widget tty)
                      (define w (tty-columns tty))
                      (define h (tty-rows tty))
                      (define layouts (tbox-layout-options widget
                                                           (sizing w 0 w)
                                                           (sizing h 0 h)))
                      (define lo (tbox-layout widget (layout-option-info (car layouts)) 0 0 w h))
                      (tbox-render! widget tty lo))
                   'write)
                  (vfil))
            ))

    (display (string-append (ansi:set-mode ansi:x11-focus-event-mode)
                            (ansi:set-mode ansi:x11-any-event-mouse-tracking-mode)
                            (ansi:set-mode ansi:x11-extended-mouse-tracking-mode))
             (terminal-output tty))
    (flush-output (terminal-output tty))

    (plumber-add-flush! (current-plumber)
                        (lambda (_handle)
                          (display (string-append
                                    (ansi:reset-mode ansi:x11-focus-event-mode)
                                    (ansi:reset-mode ansi:x11-any-event-mouse-tracking-mode)
                                    (ansi:reset-mode ansi:x11-extended-mouse-tracking-mode))
                                   (terminal-output tty))
                          (flush-output (terminal-output tty))))

    (tbox-render-toplevel! toplevel-widget tty)
    (tty-goto tty 0 0)

    (let loop ()
      (tty-flush tty)
      (tbox-render-toplevel! toplevel-widget tty)
      (tty-goto tty 0 0)
      (sync (handle-evt (tty-next-key-evt tty)
                        (lambda (k)
                          (match k
                            [(key #\q (== (set))) (void)]
                            [_
                             (tty-clear-to-eol tty)
                             (tty-display tty (format "~v" k))
                             (tty-goto tty (tty-cursor-row tty) 0)
                             (loop)])))))

    ;; (tty-shutdown!! tty)
    ;; (pretty-print toplevel-widget)
    ;; (pretty-print (tbox-layout-options toplevel-widget
    ;;                                    (sizing 80 (fill 1 0) 80)
    ;;                                    (sizing 24 (fill 1 0) 24)))
    ))
