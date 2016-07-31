#lang syndicate/actor
;; Toy file system, based on the example in the ESOP2016 submission.
;; Low-level implementation, without subconversation.

(require/activate "fs-shell.rkt")
(require/activate "fs-protocol.rkt")

(require (only-in syndicate [assert core:assert]))
(require racket/set)

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
