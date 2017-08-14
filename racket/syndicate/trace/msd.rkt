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

(define (escape-string x)
  (let* ((s (format "~a" x))
         (s (string-replace s "\\" "\\\\"))
         (s (string-replace s "\"" "\\\"")))
    (string-append "\"" s "\"")))

(let ((output-filename (getenv "SYNDICATE_MSD")))
  (when output-filename
    (let ((fh (open-output-file output-filename #:exists 'replace)))
      ;; (fprintf fh "digraph Syndicate {\n")
      ;; (plumber-add-flush! (current-plumber) (lambda (_handle)
      ;;                                         (fprintf fh "}\n")
      ;;                                         (close-output-port fh)))
      (define (write-event! . pieces)
        (write pieces fh)
        (newline fh))
      (define (msd-trace n)
        (match-define (trace-notification source sink type detail) n)
        (match* (type detail)
          [('turn-begin _process)
           (write-event! source sink 'turn-begin)]
          [('turn-end _process)
           (write-event! source sink 'turn-end)]
          [('spawn (process name _beh _state))
           (write-event! source sink 'spawn (format "~a" name))]
          [('exit exn-or-false)
           (write-event! source sink 'exit exn-or-false)]
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
      (current-trace-procedures (cons msd-trace (current-trace-procedures))))))
