#lang syndicate
;; After the Fahrenheit-to-Celsius converter example in "Fabrik - A
;; Visual Programming Environment", Ingalls, Wallace, Chow, Ludolph
;; and Doyle, OOPSLA 1988.

(struct temperature (unit value) #:prefab)
(struct set-temperature (unit value) #:prefab)

(spawn (field [temp 0])
       (assert (temperature 'C (temp)))
       (on (message (set-temperature 'C $new-temp))
           (temp new-temp))
       (on (asserted (temperature 'F $other-temp))
           (temp (exact->inexact (* (- other-temp 32) 5/9)))))

(spawn (field [temp 32])
       (assert (temperature 'F (temp)))
       (on (message (set-temperature 'F $new-temp))
           (temp new-temp))
       (on (asserted (temperature 'C $other-temp))
           (temp (exact->inexact (+ (* other-temp 9/5) 32)))))

(spawn (on (asserted (temperature $unit $value))
           (printf "Temperature in ~a is ~a\n" unit value)))

(spawn (on (asserted (observe (set-temperature _ _)))
           ;; (send! (set-temperature 'C 20))
           (send! (set-temperature 'F 90))
           ))
