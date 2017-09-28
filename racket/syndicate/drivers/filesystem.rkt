#lang syndicate
;; Filesystem change monitor driver

(provide (struct-out file-content)
         spawn-filesystem-driver)

(require syndicate/protocol/standard-relay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct file-content (name reader-proc content) #:prefab) ;; ASSERTION

;; Internal driver ground-level protocol
(struct file-changed (name) #:prefab) ;; MESSAGE
(struct file-container-changed (parent-path) #:prefab) ;; MESSAGE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-filesystem-driver)
  (spawn #:name 'filesystem-driver
         (during/spawn (observe (file-content $name $reader-proc _))
           #:name (list 'file-content name reader-proc)
           (track-file name reader-proc))
         (during (observe (inbound (file-changed $name)))
           (monitor-thread name))))

(define (read-file name reader-proc)
  (and (or (file-exists? name) (directory-exists? name))
       (reader-proc name)))

(define (path->parent-path name)
  (let-values (((parent-path _leaf _syntactically-dir?)
                (split-path (path->complete-path name))))
    parent-path))

(define (track-file name reader-proc)
  (field [content (read-file name reader-proc)])
  (assert (file-content name reader-proc (content)))
  (on (message (inbound (file-changed name)))
      (content (read-file name reader-proc)))
  ;; This horrible hack is required to work around limitations in the
  ;; OS's file-change reporting. It seems (?) as if, monitoring both
  ;; "a/b" and "a/", that only the event for "a/" will be fired when
  ;; "a/b" changes. This manifests as follows: if I monitor "a/b" and
  ;; "a/nonexistent", then when "a/b" changes, only "a/nonexistent"'s
  ;; event will fire. Therefore, I've kludged in the
  ;; `file-container-changed` message, which copes with one level of
  ;; directory hierarchy of this problem.
  ;;
  ;; TODO: Consider whether it will actually be required to listen for
  ;; file-container-changed events for ALL recursive parents of the
  ;; path of interest up to the root.
  ;;
  (on (message (inbound (file-container-changed (path->parent-path name))))
      (content (read-file name reader-proc))))

(define (monitor-thread name)
  (define control-ch (make-channel))
  (thread (lambda ()
            (define parent-path (path->parent-path name))
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
                        (handle-evt (filesystem-change-evt parent-path)
                                    (lambda (_dummy)
                                      ;; (log-info "track-file-changes ~v: directory changed" name)
                                      (send-ground-message (file-container-changed parent-path))
                                      (loop))))))))
  (on-stop (channel-put control-ch 'quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-filesystem-driver)
