#lang prospect
;; Toy file system, based on the example in the ESOP2016 submission.

(require prospect/actor)
(require prospect/drivers/timer)
(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))

(struct file (name content) #:prefab)
(struct save (file) #:prefab)
(struct delete (name) #:prefab)

(spawn-timer-driver)

(actor (forever #:collect [(files (hash))]
                (on (asserted (observe (file $name _)))
                    (printf "At least one reader exists for ~v\n" name)
                    (begin0 (until (retracted (observe (file name _)))
                                   #:collect [(content (hash-ref files name #f))]
                                   (assert (file name content))
                                   (on (message (save (file name $content))) content)
                                   (on (message (delete name)) #f))
                      (printf "No remaining readers exist for ~v\n" name)))
                (on (message (save (file $name $content))) (hash-set files name content))
                (on (message (delete $name)) (hash-remove files name))))

(define (sleep sec)
  (define timer-id (gensym 'sleep))
  (until (message (timer-expired timer-id _))
         #:init [(send! (set-timer timer-id (* sec 1000.0) 'relative))]))

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
