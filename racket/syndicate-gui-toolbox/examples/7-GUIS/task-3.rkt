#lang syndicate

(require "../../widgets.rkt")

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 

;; ---------------------------------------------------------------------------------------------------
(require gregor)

;; gregor should not raise an exception when parsing fails, but return #f
(define (to-date d) (with-handlers ([exn? (λ (_) #f)]) (parse-date d "d.M.y")))

;; ---------------------------------------------------------------------------------------------------
(define DATE0   "27.03.2014")
(define ONE     "one-way flight")
(define RETURN  "return flight")
(define CHOICES `(,ONE ,RETURN))
(define RED     "red")
(define WHITE   "white")

(define (make-field enabled)
  (spawn-text-field #:parent frame
                    #:label ""
                    #:init-value DATE0
                    #:enabled enabled))

(define frame    (spawn-frame #:label "flight booker"))
(define choice   (spawn-choice #:label "" #:parent frame #:choices CHOICES))
(define start-d  (make-field #t))
(define return-d (make-field #f))
(define book     (spawn-button #:label "Book" #:parent frame))

(spawn
 (field [*kind-flight (list-ref CHOICES 0)]  ;; one of the CHOICES
        [*start-date  (to-date DATE0)]       ;; date
        [*return-date (to-date DATE0)])      ;; date

 (define (field-cb self val date-setter!)
   (define date (to-date val))
   (cond
     [date (send! (set-text-field-background self WHITE)) (date-setter! date) (enable-book)]
     [else (send! (set-text-field-background self RED))   (enable-book #f #f)]))

 (define (enable-book [start-date (*start-date)] [return-date (*return-date)])
   (send! (enable book #f))
   (when (and start-date (date<=? (today) start-date)
              (or (and (string=? ONE (*kind-flight)))
                  (and return-date (date<=? start-date return-date))))
     (send! (enable book #t))))

 (define (enable-return-book selection)
   (*kind-flight selection)
   (send! (enable return-d (string=? RETURN (*kind-flight))))
   (enable-book))

 (on (message (text-field-update start-d $val))
     (field-cb start-d val *start-date))
 (on (message (text-field-update return-d $val))
     (field-cb return-d val *return-date))
 (on (message (choice-selection choice $sel))
     (enable-return-book sel))
 (on (message (button-press book))
     (displayln "confirmed"))

 (on-start (send! (show frame #t))
           (enable-return-book (*kind-flight))))
