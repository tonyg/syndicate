#lang syndicate
;; After [1], figure 1.
;;
;; [1] F. Sant’Anna, N. Rodriguez, R. Ierusalimschy, O. Landsiedel,
;; and P. Tsigas, “Safe system-level concurrency on
;; resource-constrained nodes,” Proc. 11th ACM Conf. Embed. Networked
;; Sens. Syst. - SenSys ’13, vol. 13, pp. 1–14, 2013.

(require/activate syndicate/drivers/timestate)

(define (led-on) (printf "LED ON\n"))
(define (led-off) (printf "led off\n"))

;;---------------------------------------------------------------------------

(spawn (on-stop (led-off))
       (on-start (let loop ()
                   (led-on)
                   (sleep 2)
                   (led-off)
                   (sleep 1)
                   (loop)))
       (stop-when-timeout 10000)) ;; in the paper, it's 1 minute = 60000 milliseconds
