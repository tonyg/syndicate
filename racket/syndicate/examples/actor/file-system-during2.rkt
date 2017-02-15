#lang syndicate/actor
;; Toy file system, based on the example in the ESOP2016 submission.
;; syndicate/actor implementation, using "during" instead of "on asserted/until retracted".

(require/activate "fs-shell.rkt")
(require/activate "fs-protocol.rkt")

(spawn (field [files (hash)])
       (during (observe (file $name _))
               (on-start (printf "At least one reader exists for ~v\n" name))
               (assert (file name (hash-ref (files) name #f)))
               (on-stop (printf "No remaining readers exist for ~v\n" name)))
       (on (message (save (file $name $content))) (files (hash-set (files) name content)))
       (on (message (delete $name)) (files (hash-remove (files) name))))
