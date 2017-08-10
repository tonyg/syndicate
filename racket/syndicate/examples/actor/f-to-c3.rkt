#lang syndicate
;; After the Fahrenheit-to-Celsius converter example in "Fabrik - A
;; Visual Programming Environment", Ingalls, Wallace, Chow, Ludolph
;; and Doyle, OOPSLA 1988.

(struct temperature (unit value) #:prefab)
(struct set-temperature (unit value) #:prefab)

(spawn #:name 'track-celsius
       (field [temp 0])
       (assert (temperature 'C (temp)))
       (on (message (set-temperature 'C $new-temp)) (temp new-temp)))

(spawn #:name 'track-fahrenheit
       (field [temp 32])
       (assert (temperature 'F (temp)))
       (on (message (set-temperature 'F $new-temp)) (temp new-temp)))

(spawn #:name 'convert-C-to-F
       (on (asserted (temperature 'C $other-temp))
           (send! (set-temperature 'F (+ (* other-temp 9/5) 32)))))

(spawn #:name 'convert-F-to-C
       (on (asserted (temperature 'F $other-temp))
           (send! (set-temperature 'C (* (- other-temp 32) 5/9)))))

(spawn (on (asserted (temperature $unit $value))
           (printf "Temperature in ~a is ~a\n" unit (exact->inexact value))))

(spawn (on (asserted (observe (set-temperature _ _)))
           ;; (send! (set-temperature 'C 20))
           (send! (set-temperature 'F 90))
           ))
