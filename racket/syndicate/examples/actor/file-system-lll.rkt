#lang syndicate
;; Toy file system, based on the example in the ESOP2016 submission.
;; Low-level implementation.

(require/activate "fs-shell.rkt")
(require/activate "fs-protocol.rkt")

(require (only-in syndicate [assert core:assert]))
(require racket/set)

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
