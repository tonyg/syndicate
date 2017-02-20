#lang syndicate/actor

(require racket/file)
(require racket/serialize)
(require racket/set)
(require operational-transformation)
(require operational-transformation/text/simple-document)

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require/activate syndicate/drivers/line-reader)

(struct snapshot-for (filename snap) #:prefab)
(struct proposed-op (filename p) #:prefab)
(struct accepted-op (filename p) #:prefab)
(struct client-seen-up-to (filename revision) #:prefab)

(define cmdline-port (make-parameter 5889))
(define cmdline-filenames (make-parameter '()))

(spawn* (for [(filename (cmdline-filenames))]
          (run-one-server filename)))

(define (run-one-server filename)
  (spawn (field [state (make-server (simple-document
                                     (if (file-exists? filename)
                                         (begin (log-info "loading ~v" filename)
                                                (file->string filename))
                                         (begin (log-info "will create ~v" filename)
                                                ""))))])
         (assert (snapshot-for filename (extract-snapshot (state))))

         (define/query-set client-seen-revs (client-seen-up-to filename $rev) rev)
         (field [oldest-needed-rev #f])
         (begin/dataflow
           (define min-rev
             (or (for/fold [(min-rev #f)] [(rev (client-seen-revs))]
                   (min (or min-rev rev) rev))
                 (server-state-revision (state))))
           (when (not (equal? (oldest-needed-rev) min-rev))
             (oldest-needed-rev min-rev)
             (state (forget-operation-history (state) min-rev))))

         (begin/dataflow
           (display-to-file (simple-document-text (server-state-document (state)))
                            filename
                            #:exists 'replace))

         (on (message (proposed-op filename $p))
             (state (incorporate-operation-from-client (state) p))
             (define sp (extract-operation (state)))
             (when sp (send! (accepted-op filename sp))))))

(spawn (define s (tcp-listener (cmdline-port)))
       (on-start (log-info "listening on port ~v" (cmdline-port)))
       (assert (advertise (observe (tcp-channel _ s _))))
       (during/spawn (advertise (tcp-channel $c s _))
                     (assert (advertise (tcp-channel s c _)))
                     (on-start (log-info "~a: connected" c))
                     (on-stop (log-info "~a: disconnected" c))
                     (connection-react c s)))

(define (connection-react c s)
  (define (output v)
    ;; (log-info "~a: sending them ~v" c v)
    (define p (open-output-bytes))
    (write (serialize v) p)
    (newline p)
    (send! (tcp-channel s c (get-output-bytes p))))

  (field [seen-up-to 0])
  (field [selected-filename #f])

  (assert #:when (selected-filename) (client-seen-up-to (selected-filename) (seen-up-to)))

  (define/query-set available-filenames (observe (proposed-op $f _)) f)
  (begin/dataflow
    (output (set->list (available-filenames))))

  (begin/dataflow
    (when (selected-filename)
      (log-info "~a: attached to file ~a" c (selected-filename))
      (let-event [(asserted (snapshot-for (selected-filename) $snapshot))]
        (output snapshot)
        (seen-up-to (server-snapshot-revision snapshot)))))
  (on #:when (selected-filename)
      (message (accepted-op (selected-filename) $p))
      (output p))

  (on (message (tcp-channel-line c s $line))
      (match (deserialize (read (open-input-bytes line)))
        [(? string? new-filename)
         (when (selected-filename) (log-info "~a: detached from file ~a" c (selected-filename)))
         (seen-up-to 0)
         (selected-filename new-filename)]
        [(? number? n) (seen-up-to n)]
        [(? pending-operation? p) (send! (proposed-op (selected-filename) p))])))

(module+ main
  (require racket/cmdline)
  (command-line
   #:once-each
   [("-p" "--port") server-port ((format "Server port (default ~v)" (cmdline-port)))
    (cmdline-port (string->number server-port))]
   #:args filenames
   (cmdline-filenames filenames)))
