#lang syndicate/actor

(require/activate syndicate/drivers/timestate)

(define (chain-step n)
  (printf "chain-step ~v\n" n)
  (actor* (sleep 1)
          (if (< n 5)
              (chain-step (+ n 1))
              (printf "done.\n"))))

(chain-step 0)
