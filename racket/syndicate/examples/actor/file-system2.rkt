#lang syndicate/actor
;; Toy file system, based on the example in the ESOP2016 submission.
;; syndicate/actor implementation, without subconversation.

(require/activate "fs-shell.rkt")
(require/activate "fs-protocol.rkt")
(require racket/set)

(spawn (field [files (hash)] [monitored (set)])
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
           (files (hash-remove (files) name))))
