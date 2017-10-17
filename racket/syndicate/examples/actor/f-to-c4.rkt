#lang syndicate
;; After the Fahrenheit-to-Celsius converter example in "Fabrik - A
;; Visual Programming Environment", Ingalls, Wallace, Chow, Ludolph
;; and Doyle, OOPSLA 1988.

(struct temperature (unit value version) #:prefab)
(struct set-temperature (unit value version) #:prefab)

(spawn #:name 'track-celsius
       (field [temp 0] [version 0])
       (assert (temperature 'C (temp) (version)))
       (on (message (set-temperature 'C $new-temp $v))
           (when (> v (version))
             (temp new-temp)
             (version v))))

(spawn #:name 'track-fahrenheit
       (field [temp 32] [version 0])
       (assert (temperature 'F (temp) (version)))
       (on (message (set-temperature 'F $new-temp $v))
           (when (> v (version))
             (temp new-temp)
             (version v))))

(spawn #:name 'convert-C-to-F
       (on (asserted (temperature 'C $other-temp $v))
           (send! (set-temperature 'F (+ (* other-temp 9/5) 32) v))))

(spawn #:name 'convert-F-to-C
       (on (asserted (temperature 'F $other-temp $v))
           (send! (set-temperature 'C (* (- other-temp 32) 5/9) v))))

(spawn (on (asserted (temperature $unit $value $v))
           (printf "Temperature in ~a at version ~a is ~a\n" unit v (exact->inexact value))))

(spawn (on (asserted (observe (set-temperature _ _ _)))
           (send! (set-temperature 'C 20 1))
           (send! (set-temperature 'F 90 2))
           ))
