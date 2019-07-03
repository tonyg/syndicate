#lang syndicate

(require "../../widgets.rkt")
(require (only-in racket/format ~a))

;; a mouse-click counter

(spawn
 (on-start

(define frame (spawn-frame #:label "Counter"))
(define pane  (spawn-horizontal-pane #:parent frame))
(define view  (spawn-text-field #:parent pane #:label "" #:init-value "0" #:enabled #f #:min-width 100))
(define _but  (spawn-button #:parent pane #:label "Count"))

(spawn
 (field [counter 0])
 (on (message (button-press _but))
     (counter (add1 (counter)))
     (send! (set-text-field view (~a (counter)))))
 (on-start
  (send! (show-frame frame #t))))))

(module+ main
  (void))
