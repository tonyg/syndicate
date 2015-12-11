#lang prospect
;; Toy file system, based on the example in the ESOP2016 submission.

(require prospect/actor)
(require (only-in racket/port read-bytes-line-evt))
(require (only-in racket/string string-trim string-split))

(struct file (name content) #:prefab)
(struct save (file) #:prefab)
(struct delete (name) #:prefab)

(%%boot
 (lambda ()
   (actor

    (actor (forever #:collect [(files (hash))]
                    (on (asserted (observe (file $name _)))
                        (printf "Someone opened ~v\n" name)
                        (until (retracted (observe (file name _)))
                               #:collect [(content (hash-ref files name #f))]
                               (assert (file name content))
                               (on (message (save (file name $content))) content)
                               (on (message (delete name)) #f))
                        ;; BUG: files has not been properly updated here
                        files)
                    (on (message (save (file $name $content))) (hash-set files name content))
                    (on (message (delete $name)) (hash-remove files name))))

    ;; Shell
    (let ((e (read-bytes-line-evt (current-input-port) 'any)))
      (define (print-prompt)
        (printf "> ")
        (flush-output))
      (actor (print-prompt)
             (until (message (external-event e (list (? eof-object? _))) #:meta-level 1)
                    (on (message (external-event e (list (? bytes? $bs))) #:meta-level 1)
                        (match (string-split (string-trim (bytes->string/utf-8 bs)))
                          [(list "open" name)
                           (actor (printf "Opening file ~a.\n" name)
                                  (until (message `(stop-watching ,name))
                                         (on (asserted (file name $contents))
                                             (printf "File ~a contains: ~v\n" name contents)))
                                  (printf "Closing file ~a.\n" name))]
                          [(list "close" name)
                           (send! `(stop-watching ,name))]
                          [(list* "write" name words)
                           (send! (save (file name words)))]
                          [_
                           (printf "I'm afraid I didn't understand that.\n")
                           (printf "Try: open filename\n")
                           (printf "     close filename\n")
                           (printf "     write filename some text goes here\n")])
                        (print-prompt)))))

    )))
