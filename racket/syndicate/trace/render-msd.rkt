#lang racket

(require pict)
(require pict/color)
(require file/convertible)
(require (only-in "util.rkt" format-pids))

(struct spacetime (space time) #:prefab)

(struct diagram-position (lane row) #:prefab)

(struct begin-swimlane (pos name) #:prefab)
(struct activate-swimlane (pos) #:prefab)
(struct deactivate-swimlane (pos) #:prefab)
(struct schedule-end-swimlane (pos) #:prefab)
(struct end-swimlane (pos) #:prefab)
(struct annotate-swimlane (pos color annotation) #:prefab)
(struct connection (from-pos to-pos) #:prefab)

(struct msd (max-lane events) #:prefab)

;;---------------------------------------------------------------------------

(define (find-unused-lane swimlane-map)
  (let ((used-lanes (list->set (hash-values swimlane-map))))
    (do ((i 0 (+ i 1)))
        ((not (set-member? used-lanes i)) i))))

(define (read-msd port)
  (define max-lane -1)
  (define swimlane-map (make-hash))
  (define name-summary (hash))
  (define events-rev '())

  (define (emit-events . es)
    (set! events-rev (foldl cons events-rev (filter values es))))

  (define (strip-meta actor-path0)
    (match actor-path0 [(list* 'meta p) p] [p p]))

  (define (translate*! actor-path0 moment)
    (define actor-path (strip-meta actor-path0))
    (diagram-position (hash-ref! swimlane-map
                                 actor-path
                                 (lambda ()
                                   (define lane (find-unused-lane swimlane-map))
                                   (when (> lane max-lane) (set! max-lane lane))
                                   (match (hash-ref name-summary actor-path #f)
                                     [#f (void)]
                                     [n (emit-events
                                         (begin-swimlane (diagram-position lane (- moment 1/2))
                                                         (format "~a =\n~a"
                                                                 (format-pids '#hash() actor-path)
                                                                 n)))])
                                   lane))
                      moment))

  (define (vacate-lane! point)
    (define actor-path (strip-meta (spacetime-space point)))
    (hash-remove! swimlane-map
                  (match actor-path
                    [(list* 'meta p) p]
                    [_ actor-path])))

  (define (translate! point)
    (translate*! (spacetime-space point) (spacetime-time point)))

  (define (connection* source sink)
    (and source (connection (translate! source) (translate! sink))))

  (let loop ()
    (match (read port)
      [(? eof-object?) (msd max-lane (reverse events-rev))]
      [input
       (match input
         [(list _ _ 'name-summary names-alist)
          (set! name-summary (make-immutable-hash names-alist))]
         [(list source sink 'turn-begin)
          (emit-events (activate-swimlane (translate! sink)))]
         [(list source sink 'turn-end)
          (emit-events (deactivate-swimlane (translate! sink)))]
         [(list source sink 'spawn name)
          (emit-events (begin-swimlane (translate! sink)
                                       (format "~a =\n~a"
                                               (format-pids '#hash() (spacetime-space sink))
                                               name))
                       (connection* source sink))]
         [(list source sink 'exit _exn-or-false)
          (emit-events (schedule-end-swimlane (translate! sink)))]
         [(list source sink 'actions-produced count)
          (emit-events (annotate-swimlane (translate! sink)
                                          ACTION-COLOR
                                          (match count
                                            [1 "1 action"]
                                            [n (format "~a actions" n)])))]
         [(list source sink 'action-interpreted _ desc)
          (define shifted-sink
            (if source
                (spacetime (spacetime-space source) (spacetime-time sink))
                sink))
          (emit-events (annotate-swimlane (translate! shifted-sink) ACTION-COLOR desc)
                       (connection* source shifted-sink))]
         [(list source sink 'quit)
          (define shifted-sink
            (if source
                (spacetime (spacetime-space source) (spacetime-time sink))
                sink))
          (emit-events (begin0 (end-swimlane (translate! shifted-sink))
                         (when source (vacate-lane! shifted-sink))))]
         [(list direct-cause recipient 'event _ desc indirect-cause doubly-indirect-paths)
          (apply emit-events
                 (annotate-swimlane (translate! recipient) EVENT-COLOR desc)
                 ;; (connection* direct-cause recipient)
                 ;; (connection* indirect-cause recipient)
                 (map (lambda (doubly-indirect-path)
                        (connection (translate*! doubly-indirect-path (spacetime-time direct-cause))
                                    (translate! recipient)))
                      doubly-indirect-paths))])
       (loop)])))

;;---------------------------------------------------------------------------

(define N (* pi 1/2))
(define S (* pi -1/2))
(define E (* pi 0))
(define W (* pi 1))

(define NE (* pi 1/4))
(define SE (* pi -1/4))
(define NW (* pi 3/4))
(define SW (* pi -3/4))

(define ENE (* pi 1/8))
(define ESE (* pi -1/8))
(define WNW (* pi 7/8))
(define WSW (* pi -7/8))

;; A SwimlaneState is
;; - a (labelled-cell String ColorString SwimlaneState), representing
;;   a fresh annotation on a possibly-new swimlane
;; - 'inactive, an occupied but inactive lane
;; - 'active, an occupied and active lane
;; - 'terminating, an occupied but soon-to-be-free lane
;; - #f, an unoccupied lane

(struct labelled-cell (label color underlying-state) #:prefab)

(define (hash-set-or-remove h k v)
  (if v
      (hash-set h k v)
      (hash-remove h k)))

(define (reset-label v)
  (match v
    [(labelled-cell _ _ underlying-state) underlying-state]
    [_ v]))

(define (reset-statemap statemap)
  (for/hash [((k v) (in-hash statemap))]
    (values k (reset-label v))))

(define (update-statemap statemaps pos new-state)
  (match-define (list previous-row table) statemaps)
  (match-define (diagram-position lane row) pos)

  (define old-statemap
    (hash-ref table row (lambda () (reset-statemap (hash-ref table previous-row hash)))))

  (define new-statemap
    (hash-set-or-remove old-statemap
                        lane
                        (match new-state
                          [(list color annotation)
                           (labelled-cell annotation color (hash-ref old-statemap lane 'inactive))]
                          [_ new-state])))

  (list row
        (hash-set-or-remove table
                            row
                            (if (hash-empty? new-statemap) #f new-statemap))))

(define (update-swimlane event statemaps)
  (match event
    [(begin-swimlane pos name)
     (update-statemap statemaps pos (list LABEL-COLOR name))]
    [(activate-swimlane pos)
     (update-statemap statemaps pos 'active)]
    [(deactivate-swimlane pos)
     (update-statemap statemaps pos 'inactive)]
    [(schedule-end-swimlane pos)
     (update-statemap statemaps pos 'terminating)]
    [(end-swimlane pos)
     (update-statemap statemaps pos #f)]
    [(annotate-swimlane pos color annotation)
     (update-statemap statemaps pos (list color annotation))]))

(define WIDTH (make-parameter 50))
(define HEIGHT (make-parameter 20))
(define GAP (make-parameter 20))
(define ACTIVE-WIDTH 10)

(define ACTION-COLOR "white")
(define EVENT-COLOR "orange")
(define LABEL-COLOR "palegreen")

(define (fill-out height p)
  (ct-superimpose (blank (WIDTH) height)
                  (if p
                      (inset p (* -1/2 (pict-width p)) 0)
                      (blank 0))))

(define (transpose xs)
  (apply map list xs))

(define (render-underlay max-lane prev-row current-row next-row height)
  (for/list [(lane (+ max-lane 1))]
    (fill-out height
              (match (match (hash-ref current-row lane #f)
                       [(labelled-cell s _ u) u]
                       [u u])
                ['inactive (filled-rectangle #:color "gray"
                                             #:draw-border? #f
                                             (/ ACTIVE-WIDTH 2)
                                             height)]
                ['active
                 (define prev-state (hash-ref prev-row lane #f))
                 (define next-state (hash-ref next-row lane #f))
                 (define (trunk height)
                   (hb-append 0
                              (vline 1 height)
                              (blank (- ACTIVE-WIDTH 2) height)
                              (vline 1 height)))
                 (define bar (hline (- ACTIVE-WIDTH 1) 1))
                 (match* (prev-state next-state)
                   [('active 'active) (trunk height)]
                   [('active _) (vl-append 0 (trunk (- height 1)) bar)]
                   [(_ 'active) (vl-append 0 bar (trunk (- height 1)))]
                   [(_ _) (vl-append 0 bar (trunk (- height 2)) bar)])]
                ['terminating (vline 1 height #:segment 2)]
                [#f (blank 0)]))))

(define (render-overlay max-lane current-row)
  (for/list [(lane (+ max-lane 1))]
    (fill-out (HEIGHT)
              (match (hash-ref current-row lane #f)
                [(labelled-cell s color u)
                 (define para (apply vl-append 0
                                     (map (lambda (s)
                                            (define limit 200)
                                            (text (if (> (string-length s) limit)
                                                      (string-append (substring s 0 limit) "...")
                                                      s)
                                                  'modern))
                                          (string-split s "\n"))))
                 (vc-append
                  (disk 4)
                  (frame
                   (cc-superimpose
                    (filled-rectangle #:color color
                                      #:draw-border? #f
                                      (+ (pict-width para) 8)
                                      (+ (pict-height para) 8))
                    para)))]
                [_ #f]))))

(define (msd->pict m)
  (match-define (msd max-lane all-events) m)
  (define-values (connections events) (partition connection? all-events))
  (match-define (list _row-count statemaps) (foldl update-swimlane (list 0 (hash)) events))
  (define rows (sort (hash->list statemaps) < #:key car))
  (define dummy-initial-row (cons (- (car (car rows)) 1) (hash)))
  (define dummy-final-row (match (last rows)
                            [(cons final-row final-statemap)
                             (cons (+ final-row 1) (reset-statemap final-statemap))]))
  (define row-triples
    (map (lambda (a b c)
           (list (car b)
                 (reset-statemap (cdr a))
                 (cdr b)
                 (reset-statemap (cdr c))))
         (cons dummy-initial-row rows)
         (append rows (list dummy-final-row))
         (append (cdr rows) (list dummy-final-row dummy-final-row))))
  (define over-and-unders
    (map (match-lambda
           [(list row-number prev-row current-row next-row)
            (define overlay (render-overlay max-lane current-row))
            (define height (+ (GAP) (apply max (map pict-height overlay))))
            (define underlay (render-underlay max-lane prev-row current-row next-row height))
            (list row-number underlay overlay)])
         row-triples))
  (define overlay-index (for/hash [(entry over-and-unders)]
                          (values (car entry) (caddr entry))))
  (define base-pict
    (apply vl-append 0
           (map (lambda (over-and-under)
                  (ct-superimpose (apply hb-append 4 (cadr over-and-under))
                                  (apply hb-append 4 (caddr over-and-under))))
                over-and-unders)))
  (for/fold [(base-pict base-pict)]
            [(c connections)]
    (match-define (connection (diagram-position from-lane from-row)
                              (diagram-position to-lane to-row))
      c)
    (if (and (hash-has-key? overlay-index from-row)
             (hash-has-key? overlay-index to-row))
        (pin-arrow-line 10 base-pict
                        (list-ref (hash-ref overlay-index from-row) from-lane)
                        cb-find
                        (list-ref (hash-ref overlay-index to-row) to-lane)
                        ct-find
                        #:start-angle (if (>= to-lane from-lane) SE SW)
                        #:start-pull 1/10
                        #:end-angle (if (<= to-lane from-lane) SW SE)
                        #:end-pull 1/10)
        base-pict)))

(define (render p #:target [target (string->symbol (or (getenv "VM_PICTURES_TARGET") "eps"))])
  (define (final-border) 1)
  (define final-pict (cc-superimpose (blank (+ (pict-width p) (* 2 (final-border)))
					    (+ (pict-height p) (* 2 (final-border))))
				     (panorama p)))
  (case target
    [(screen)
     ;; FFS. This connects to the display even if you don't use it.
     (local-require racket/gui/base)
     (show-pict final-pict 800 600)
     ;; (log-error "You need to uncomment a couple of lines in vm-pictures.rkt")
     (void)]
    [(png)
     (display (convert final-pict 'png-bytes))]
    [(png@2x)
     (display (convert final-pict 'png@2x-bytes))]
    [(svg)
     (display (convert final-pict 'svg-bytes))]
    [(eps)
     (display (convert final-pict 'eps-bytes))]
    [(pdf)
     (display (convert final-pict 'pdf-bytes))]))

(module+ main
  (require racket/cmdline)
  (define *scale* 1)
  (define *target* 'screen)
  (define filename
    (command-line
     #:once-each
     [("-s" "--scale") scale "Rescales output; 1 = 100%"
      (set! *scale* (string->number scale))]
     [("-t" "--target") target "Choose target: screen, png, png@2x, svg, eps, pdf"
      (set! *target* (string->symbol target))]
     [("--width") width "Width of swimlane cells (default: 50)"
      (WIDTH (string->number width))]
     [("--height") height "Minimum height of rows (default: 20)"
      (HEIGHT (string->number height))]
     [("--gap") gap "Extra space between rows (default: 20)"
      (GAP (string->number gap))]
     #:args (filename)
     filename))
  (render (scale (msd->pict (if (equal? filename "-")
                                (read-msd (current-input-port))
                                (call-with-input-file filename read-msd)))
                 *scale*)
          #:target *target*))
