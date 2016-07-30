#lang syndicate/actor
;; Toy file system, based on the example in the ESOP2016 submission.
;; Low-level implementation, without subconversation.

(require (only-in syndicate [assert core:assert]))
(require/activate syndicate/drivers/timer)
(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))
(require racket/set)

(struct file (name content) #:prefab)
(struct save (file) #:prefab)
(struct delete (name) #:prefab)

(struct fs-state (files monitored) #:prefab)

(define (update-file state name new-content)
  (transition (struct-copy fs-state state
                           [files (if new-content
                                      (hash-set (fs-state-files state) name new-content)
                                      (hash-remove (fs-state-files state) name))])
              (if (set-member? (fs-state-monitored state) name)
                  (patch-seq (retract (file name (hash-ref (fs-state-files state) name #f)))
                             (core:assert (file name new-content)))
                  '())))

(define (file-system-event-handler e state)
  (match e
    [(? patch? p)
     (define monitored-to-add
       (for-trie/set [((observe (file $name _)) (patch-added p))] name))
     (define monitored-to-remove
       (for-trie/set [((observe (file $name _)) (patch-removed p))] name))
     (transition (struct-copy fs-state state
                              [monitored (set-subtract (set-union (fs-state-monitored state)
                                                                  monitored-to-add)
                                                       monitored-to-remove)])
                 (list (for/list [(name monitored-to-add)]
                         (printf "At least one reader exists for ~v\n" name)
                         (core:assert (file name (hash-ref (fs-state-files state) name #f))))
                       (for/list [(name monitored-to-remove)]
                         (printf "No remaining readers exist for ~v\n" name)
                         (retract (file name ?)))))]
    [(message (save (file name new-content)))
     (update-file state name new-content)]
    [(message (delete name))
     (update-file state name #f)]
    [_ #f]))

(spawn file-system-event-handler
       (fs-state (hash) (set))
       (patch-seq (sub (observe (file ? ?)))
                  (sub (save (file ? ?)))
                  (sub (delete ?))))

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
