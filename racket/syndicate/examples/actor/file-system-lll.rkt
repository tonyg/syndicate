#lang syndicate
;; Toy file system, based on the example in the ESOP2016 submission.
;; Low-level implementation.

(require (only-in syndicate [assert core:assert]))
(require syndicate/actor)
(require syndicate/drivers/timer)
(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))
(require racket/set)

(struct file (name content) #:prefab)
(struct save (file) #:prefab)
(struct delete (name) #:prefab)

(spawn-timer-driver)

(define (file-system-event-handler e files)
  (match-event e
    [(? patch? p)
     (transition files
                 (for-trie/list [((observe (file $name _)) (patch-added p))]
                   (printf "At least one reader exists for ~v\n" name)
                   (define initial-content (hash-ref files name #f))
                   (spawn (file-observation-event-handler name)
                          initial-content
                          (patch-seq (core:assert (file name initial-content))
                                     (sub (observe (file name ?)))
                                     (sub (save (file name ?)))
                                     (sub (delete name))))))]
    [(message (save (file name new-content)))
     (transition (hash-set files name new-content) '())]
    [(message (delete name))
     (transition (hash-remove files name) '())]))

(spawn file-system-event-handler
       (hash)
       (patch-seq (sub (observe (file ? ?)))
                  (sub (save (file ? ?)))
                  (sub (delete ?))))

(define (update-file old-content name new-content)
  (transition new-content
              (patch-seq (retract (file name old-content))
                         (core:assert (file name new-content)))))

(define ((file-observation-event-handler name) e content)
  (match-event e
    [(? patch? p)
     (when (not (set-empty? (project-assertions (patch-removed p) (observe (file (?!) ?)))))
       (printf "No remaining readers exist for ~v\n" name)
       (quit))]
    [(message (save (file (== name) new-content)))
     (update-file content name new-content)]
    [(message (delete (== name)))
     (update-file content name #f)]))

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
         (until (message (external-event e (list (? eof-object? _))) #:meta-level 1)
                (on (message (external-event e (list (? bytes? $bs))) #:meta-level 1)
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
