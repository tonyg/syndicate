#lang syndicate/actor

(require racket/file)
(require racket/serialize)
(require operational-transformation)
(require operational-transformation/text/simple-document)

(require/activate syndicate/drivers/tcp)
(require/activate syndicate/drivers/line-reader)

(struct file-being-edited (name) #:prefab)
(struct proposed-op (p) #:prefab)
(struct accepted-op (p) #:prefab)

(define cmdline-port (make-parameter 5888))
(define cmdline-filename (make-parameter "info.rkt"))

(actor (react (field [state (make-server (simple-document
                                          (if (file-exists? (cmdline-filename))
                                              (begin (log-info "loading ~v" (cmdline-filename))
                                                     (file->string (cmdline-filename)))
                                              (begin (log-info "will create ~v" (cmdline-filename))
                                                     ""))))])
              (assert (extract-snapshot (state)))

              (begin/dataflow
                (display-to-file (simple-document-text (server-state-document (state)))
                                 (cmdline-filename)
                                 #:exists 'replace))

              (on (message (proposed-op $p))
                  (state (incorporate-operation-from-client (state) p))
                  (define sp (extract-operation (state)))
                  (when sp (send! (accepted-op sp))))))

(actor (define s (tcp-listener (cmdline-port)))
       (log-info "listening on port ~v" (cmdline-port))
       (forever (assert (advertise (observe (tcp-channel _ s _))))
                (during/actor (advertise (tcp-channel $c s _))
                              (assert (advertise (tcp-channel s c _)))
                              (on-start (log-info "~a: connected" c))
                              (on-stop (log-info "~a: disconnected" c))
                              (connection-react c s (cmdline-filename)))))

(define (connection-react c s filename)
  (define (output v)
    ;; (log-info "~a: sending them ~v" c v)
    (define p (open-output-bytes))
    (write (serialize v) p)
    (newline p)
    (send! (tcp-channel s c (get-output-bytes p))))

  (on-start
   (output filename)
   (let-event [(asserted ($ snapshot (server-snapshot _ _)))]
              (output snapshot)
              (react (on (message (accepted-op $p))
                         (output p)))))

  (on (message (tcp-channel-line c s $line))
      (send! (proposed-op (deserialize (read (open-input-bytes line)))))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:once-each
   [("-p" "--port") server-port ((format "Server port (default ~v)" (cmdline-port)))
    (cmdline-port server-port)]
   #:args (filename)
   (cmdline-filename filename)))
