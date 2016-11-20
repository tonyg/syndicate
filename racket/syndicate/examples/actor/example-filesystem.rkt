#lang syndicate/actor

(require/activate syndicate/drivers/filesystem)
(require racket/file)

(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))

(let ((e (read-bytes-line-evt (current-input-port) 'any)))
  (actor #:name 'monitor-shell
         (stop-when (message (inbound (external-event e (list (? eof-object? _)))))
                    (send! (list "close" ?)))
         (on (message (inbound (external-event e (list (? bytes? $command-bytes)))))
             (send! (string-split (string-trim (bytes->string/utf-8 command-bytes)))))))

(actor #:name 'monitor-opener
       (on (message (list "open" $name))
           (actor #:name (list 'monitor name)
                  (stop-when (message (list "close" name)))
                  (on (asserted (file-content name file->bytes $bs))
                      (log-info "~a: ~v" name bs)))))
