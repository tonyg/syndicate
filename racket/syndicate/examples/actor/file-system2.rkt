#lang syndicate/actor
;; Toy file system, based on the example in the ESOP2016 submission.
;; syndicate/actor implementation, without subconversation.

(require/activate syndicate/drivers/timer)
(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))
(require racket/set)

(struct file (name content) #:prefab)
(struct save (file) #:prefab)
(struct delete (name) #:prefab)

(actor (react (field [files (hash)] [monitored (set)])
              (on (asserted (observe (file $name _)))
                  (printf "At least one reader exists for ~v\n" name)
                  (assert! (file name (hash-ref (files) name #f)))
                  (monitored (set-add (monitored) name)))
              (on (retracted (observe (file $name _)))
                  (printf "No remaining readers exist for ~v\n" name)
                  (retract! (file name (hash-ref (files) name #f)))
                  (monitored (set-remove (monitored) name)))
              (on (message (save (file $name $content)))
                  (when (set-member? (monitored) name)
                    (retract! (file name (hash-ref (files) name #f)))
                    (assert! (file name content)))
                  (files (hash-set (files) name content)))
              (on (message (delete $name))
                  (when (set-member? (monitored) name)
                    (retract! (file name (hash-ref (files) name #f)))
                    (assert! (file name #f)))
                  (files (hash-remove (files) name)))))

(define (sleep sec)
  (define timer-id (gensym 'sleep))
  (until (message (timer-expired timer-id _))
         (on-start (send! (set-timer timer-id (* sec 1000.0) 'relative)))))

;; Shell
(let ((e (read-bytes-line-evt (current-input-port) 'any)))
  (define (print-prompt)
    (printf "> ")
    (flush-output))
  (define reader-count 0)
  (define (generate-reader-id)
    (begin0 reader-count
      (set! reader-count (+ reader-count 1))))
  (actor (print-prompt)
         (until (message (inbound (external-event e (list (? eof-object? _)))))
                (on (message (inbound (external-event e (list (? bytes? $bs)))))
                    (match (string-split (string-trim (bytes->string/utf-8 bs)))
                      [(list "open" name)
                       (define reader-id (generate-reader-id))
                       (actor (printf "Reader ~a opening file ~v.\n" reader-id name)
                              (until (message `(stop-watching ,name))
                                     (on (asserted (file name $contents))
                                         (printf "Reader ~a sees that ~v contains: ~v\n"
                                                 reader-id
                                                 name
                                                 contents)))
                              (printf "Reader ~a closing file ~v.\n" reader-id name))]
                      [(list "close" name)
                       (send! `(stop-watching ,name))]
                      [(list* "write" name words)
                       (send! (save (file name words)))]
                      [(list "delete" name)
                       (send! (delete name))]
                      [_
                       (printf "I'm afraid I didn't understand that.\n")
                       (printf "Try: open filename\n")
                       (printf "     close filename\n")
                       (printf "     write filename some text goes here\n")
                       (printf "     delete filename\n")])
                    (sleep 0.1)
                    (print-prompt)))))
