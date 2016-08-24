#lang syndicate/actor
;; Toy file system, based on the example in the ESOP2016 submission.
;; syndicate/actor implementation.

(require/activate "fs-shell.rkt")
(require/activate "fs-protocol.rkt")

(actor (field [files (hash)])
       (on (asserted (observe (file $name _)))
           (printf "At least one reader exists for ~v\n" name)
           (until (retracted (observe (file name _)))
                  (field [content (hash-ref (files) name #f)])
                  (assert (file name (content)))
                  (on (message (save (file name $new-content))) (content new-content))
                  (on (message (delete name)) (content #f)))
           (printf "No remaining readers exist for ~v\n" name))
       (on (message (save (file $name $content))) (files (hash-set (files) name content)))
       (on (message (delete $name)) (files (hash-remove (files) name))))
