#lang syndicate
;; Automagic "make"-like utility.

(require/activate syndicate/drivers/filesystem)
(require racket/string)
(require racket/system)
(require file/sha1)

(define (file->sha1 p)
  (call-with-input-file p sha1))

(spawn (during (observe (file-content $name _ _))
         (when (not (string-suffix? name ".c"))
           (define name.c (string-append name ".c"))
           (on-start (printf "Tracking ~a, to see if we can use it to build ~a\n" name.c name))
           (during (file-content name.c file->sha1 $hash) ;; nb. $hash, not _
             (on-start
              (if hash
                  (begin (printf "~a has changed hash to ~a, recompiling\n" name.c hash)
                         (system* (find-executable-path "cc") "-o" name name.c))
                  (printf "~a doesn't exist.\n" name.c)))))))

(spawn (on (asserted (file-content "." directory-list $files))
           (for [(name-path (in-list files))]
             (match (path->string name-path)
               [(pregexp #px"(.*)\\.c" (list _ name))
                (assert! (observe (file-content name file-exists? #t)))]
               [_ (void)]))))
