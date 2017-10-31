#lang syndicate

(require/activate syndicate/drivers/timestate)

(define (foo)
  (printf "Sleeping foo\n")
  (sleep 2)
  (printf "Done foo\n"))

(define (bar)
  (printf "Sleeping bar\n")
  (sleep 1)
  (printf "Done bar\n"))

(define (cleanup-after . script-thunks)
  (react (on-stop (printf "Cleaning up\n"))
         (for [(script-thunk script-thunks)]
           (on-start (script-thunk)))))

(spawn (on-start (printf "Starting process\n"))
       (on-stop (printf "Stopping process\n"))
       (on-start (cleanup-after foo bar)))
