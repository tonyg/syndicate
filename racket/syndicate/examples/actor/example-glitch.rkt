#lang syndicate/actor
;; Demonstrate FRP-like "glitching"
;;
;; Based on an example from the FRP survey of Bainomugisha et al.
;; 2013:
;;
;;     var1 = 1
;;     var2 = var1 * 1
;;     var3 = var1 + var2
;;
;; Sample output; notice doubled 'var3 printing:
;;
;; 'var1 = 1
;; 'var2 = 1
;; 'var3 = 2
;; ---
;; 'var1 = 2
;; 'var2 = 2
;; 'var3 = 3
;; 'var3 = 4
;; ---
;; 'var1 = 3
;; 'var2 = 3
;; 'var3 = 5
;; 'var3 = 6
;; ---
;; 'var1 = 4
;; 'var2 = 4
;; 'var3 = 7
;; 'var3 = 8
;; ---

(require/activate syndicate/drivers/timestate)

(spawn (field [var1 1])
       (assert (list 'var1 (var1)))
       (on (message 'tick)
           (var1 (+ (var1) 1))))

(spawn (during (list 'var1 $v)
         (assert (list 'var2 (* v 1)))))

(spawn (during (list 'var1 $v1)
         (during (list 'var2 $v2)
           (assert (list 'var3 (+ v1 v2))))))

(spawn (on (asserted (list $k $v))
           (printf "~v = ~v\n" k v)))

(spawn* (until (asserted (observe 'tick)))
        (sleep 0.1)
        (printf "---\n")
        (send! 'tick)
        (sleep 0.1)
        (printf "---\n")
        (send! 'tick)
        (sleep 0.1)
        (printf "---\n")
        (send! 'tick)
        (sleep 0.1)
        (printf "---\n"))
