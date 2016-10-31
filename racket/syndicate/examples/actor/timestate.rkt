#lang syndicate/actor

(require/activate syndicate/drivers/timestate)

(actor #:name 'demo-timeout
       (on-start (printf "Starting demo-timeout\n"))
       (on-stop (printf "Stopping demo-timeout\n"))
       (stop-when-timeout 3000 (printf "Three second timeout fired\n")))

(actor #:name 'demo-later-than
       (on-start (printf "Starting demo-later-than\n"))
       (on-stop (printf "Stopping demo-later-than\n"))
       (field [deadline (+ (current-inexact-milliseconds) 5000)])
       (stop-when (asserted (later-than (deadline)))
                  (printf "Deadline expired\n")))

(actor #:name 'demo-updating-later-than
       (field [deadline (current-inexact-milliseconds)])
       (field [counter 0])
       (on #:when (< (counter) 10) (asserted (later-than (deadline)))
           (printf "Tick ~v\n" (counter))
           (counter (+ (counter) 1))
           (deadline (+ (deadline) 1000))))
