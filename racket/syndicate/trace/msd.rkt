#lang racket/base

(require racket/match)
(require racket/set)
(require racket/string)
(require racket/pretty)
(require "../core.rkt")
(require "../tset.rkt")
(require "../patch.rkt")
(require "../trace.rkt")
(require "util.rkt")

(define-logger syndicate/trace/msd)

(let ((output-filename (getenv "SYNDICATE_MSD")))
  (when output-filename
    (define names (make-hash (list (cons '() "'ground"))))
    (define (open-output cause)
      (log-syndicate/trace/msd-warning "~a: opening trace file ~a" cause output-filename)
      (define fh (open-output-file output-filename #:exists 'replace))
      (write (list #f #f 'name-summary
                   (for/list [((actor-path name) (in-hash names))]
                     (cons actor-path (format "~a" name))))
             fh)
      (newline fh)
      fh)
    (let ((fh (open-output "Startup")))
      (define (write-event! . pieces)
        (let ((fh fh)) ;; avoid non-atomic access; see thread below
          (when fh
            (write pieces fh)
            (newline fh))))
      (define (msd-trace n)
        (match-define (trace-notification source sink type detail) n)
        (match* (type detail)
          [('turn-begin _process)
           (write-event! source sink 'turn-begin)]
          [('turn-end _process)
           (write-event! source sink 'turn-end)]
          [('spawn (process name _beh _state))
           (hash-set! names (spacetime-space sink) name)
           (write-event! source sink 'spawn (format "~a" name))]
          [('exit exn-or-false)
           (write-event! source sink 'exit (format "~a" exn-or-false))]
          [('actions-produced actions)
           (when (positive? (length actions))
             (write-event! source sink 'actions-produced (length actions)))]
          [('action-interpreted (? patch? p))
           (write-event! source sink 'action-interpreted
                         'patch
                         (patch->pretty-string (label-patch p #t)))]
          [('action-interpreted (message body))
           (write-event! source sink 'action-interpreted
                         'message
                         (pretty-format body))]
          [('action-interpreted 'quit)
           (hash-remove! names (spacetime-space source))
           (write-event! source sink 'quit)]
          [('event (list cause (? patch? p)))
           (match (spacetime-space sink)
             ['()
              (write-event! source sink 'event
                            'patch
                            (patch->pretty-string p)
                            cause
                            (list (spacetime-space cause)))]
             [(cons _ context-path)
              (write-event! source sink 'event
                            'patch
                            (format-patch '#hash() context-path p)
                            cause
                            (set-map (extract-patch-pids p)
                                     (lambda (local-pid) (cons local-pid context-path))))])]
          [('event (list cause (message body)))
           (write-event! source sink 'event
                         'message
                         (pretty-format body)
                         cause
                         (list (spacetime-space cause)))]
          [('event (list _cause #f)) ;; cause will be #f
           (void)]))
      (define ch (make-channel))
      ;; ^ ?!?!?!! Why do I have to do this to avoid problems loading
      ;; the unix-signals package??? Is there a Racket-level race in
      ;; namespace-management code???
      (thread (lambda ()
                (define next-signal-evt (check-for-unix-signals-support!))
                (channel-put ch (void))
                (when next-signal-evt
                  (log-syndicate/trace/msd-info "SIGUSR1 toggles/resets trace file ~a"
                                                output-filename)
                  (let loop ()
                    (match (sync next-signal-evt)
                      ['SIGUSR1
                       (set! fh (cond
                                  [fh
                                   (log-syndicate/trace/msd-warning "SIGUSR1: closing trace file ~a"
                                                                    output-filename)
                                   (close-output-port fh)
                                   #f]
                                  [else
                                   (open-output "SIGUSR1")]))]
                      [_ (void)])
                    (loop)))))
      (channel-get ch)
      (current-trace-procedures (cons msd-trace (current-trace-procedures))))))
