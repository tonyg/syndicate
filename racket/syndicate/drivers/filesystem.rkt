#lang syndicate/actor
;; Filesystem change monitor driver

(provide (struct-out file-content)
         spawn-filesystem-driver)

(require syndicate/protocol/standard-relay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct file-content (name reader-proc content) #:prefab) ;; ASSERTION

(struct file-changed (name) #:prefab) ;; MESSAGE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-filesystem-driver)
  (actor #:name 'filesystem-driver
         (during/actor (observe (file-content $name $reader-proc _))
           #:name (list 'file-content name reader-proc)
           (track-file name reader-proc))))

(define (read-file name reader-proc)
  (and (or (file-exists? name) (directory-exists? name))
       (reader-proc name)))

(define (track-file name reader-proc)
  (define control-ch (make-channel))
  (thread (lambda () (track-file-changes name control-ch)))

  (field [content (read-file name reader-proc)])

  (assert (file-content name reader-proc (content)))

  (on (message (inbound (file-changed name)))
      (content (read-file name reader-proc)))

  (on-stop (channel-put control-ch 'quit)))

(define (track-file-changes name control-ch)
  (let loop ()
    (sync (handle-evt control-ch
                      (lambda (msg)
                        ;; (log-info "track-file-changes ~v: ~v" name msg)
                        (match msg
                          ['quit (void)])))
          (if (or (file-exists? name) (directory-exists? name)) ;; TODO: TOCTTOU :-(
              (handle-evt (filesystem-change-evt name)
                          (lambda (_dummy)
                            ;; (log-info "track-file-changes ~v: changed" name)
                            (send-ground-message (file-changed name))
                            (loop)))
              (let-values (((parent-path _leaf _syntactically-dir?)
                            (split-path (path->complete-path name))))
                (handle-evt (filesystem-change-evt parent-path)
                            (lambda (_dummy)
                              ;; (log-info "track-file-changes ~v: directory changed" name)
                              (send-ground-message (file-changed name))
                              (loop))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-filesystem-driver)
