#lang syndicate

(provide spawn-frame
         spawn-horizontal-pane
         spawn-vertical-pane
         spawn-text-field
         spawn-button
         spawn-choice
         spawn-gauge
         spawn-slider
         spawn-list-box
         (struct-out frame@)
         (struct-out show-frame)
         (struct-out horizontal-pane@)
         (struct-out vertical-pane@)
         (struct-out text-field@)
         (struct-out set-text-field)
         (struct-out button@)
         (struct-out button-press)
         (struct-out set-text-field-background)
         (struct-out text-field-update)
         (struct-out choice@)
         (struct-out choice-selection)
         (struct-out set-selection)
         (struct-out enable)
         (struct-out gauge@)
         (struct-out set-gauge-value)
         (struct-out slider@)
         (struct-out slider-update)
         (struct-out list-box@)
         (struct-out list-box-selection)
         (struct-out set-list-box-choices))

(require (only-in racket/class
                  new
                  send
                  make-object))
(require racket/gui/base)

;; an ID is a (Sealof Any)
;; an Alignment is a (List (U 'left 'center 'right) (U 'top 'center 'bottom))

(message-struct enable (id val))

(assertion-struct frame@ (id))
(message-struct show-frame (id value))

(assertion-struct horizontal-pane@ (id))
(assertion-struct vertical-pane@ (id))

(assertion-struct text-field@ (id value))
(message-struct set-text-field (id value))
(message-struct set-text-field-background (id color))
(message-struct text-field-update (id value))

(assertion-struct button@ (id))
(message-struct button-press (id))

(assertion-struct choice@ (id selection))
(message-struct choice-selection (id val))
(message-struct set-selection (id idx))

(assertion-struct gauge@ (id))
(message-struct set-gauge-value (id value))

(assertion-struct slider@ (id value))
(message-struct slider-update (id value))

(assertion-struct list-box@ (id idx))
(message-struct list-box-selection (id idx))
(message-struct set-list-box-choices (id choices))

(define (enable/disable-handler self my-id)
  (on (message (enable my-id $val))
      (send self enable val)))

;; String -> ID
(define (spawn-frame #:label label)
  (define frame
    (parameterize ((current-eventspace (make-eventspace)))
      (new frame% [label label])))
  (define id (seal frame))
  (spawn
   (assert (frame@ id))
   (on (message (show-frame id $val))
       (send frame show val)))
  id)

;; ID -> ID
(define (spawn-horizontal-pane #:parent parent
                               #:border [border 0]
                               #:alignment [alignment '(left center)])
  (define parent-component (seal-contents parent))
  (define pane (new horizontal-pane%
                    [parent parent-component]
                    [border border]
                    [alignment alignment]))
  (define id (seal pane))

  (spawn
   (assert (horizontal-pane@ id)))

  id)

;; ID Alignment -> ID
(define (spawn-vertical-pane #:parent parent
                             #:alignment [alignment '(center top)])
  (define parent-component (seal-contents parent))
  (define pane (new vertical-pane%
                    [parent parent-component]
                    [alignment alignment]))
  (define id (seal pane))

  (spawn
   (assert (vertical-pane@ id)))

  id)

; ID String String Bool Nat -> ID
(define (spawn-text-field #:parent parent
                          #:label label
                          #:init-value init
                          #:enabled [enabled? #t]
                          #:min-width [min-width #f])
  (define parent-component (seal-contents parent))

  (define (inject-text-field-update! _ evt)
    (send-ground-message (text-field-update id (send tf get-value))))

  (define tf (new text-field%
                  [parent parent-component]
                  [label label]
                  [init-value init]
                  [enabled enabled?]
                  [min-width min-width]
                  [callback inject-text-field-update!]))
  (define id (seal tf))

  (spawn
   (field [val (send tf get-value)])
   (assert (text-field@ id (val)))
   (enable/disable-handler tf id)
   (on (message (set-text-field id $value))
       (send tf set-value value)
       (val value))
   (on (message (set-text-field-background id $color))
       (define c (make-object color% color))
       (send tf set-field-background c))
   (on (message (inbound (text-field-update id $value)))
       (val (send tf get-value))
       (send! (text-field-update id (val)))))

  id)

;; ID String -> ID
(define (spawn-button #:parent parent
                      #:label label)
  (define (inject-button-press! b e)
    (send-ground-message (button-press id)))
  (define parent-component (seal-contents parent))
  (define but (new button%
                   [parent parent-component]
                   [label label]
                   [callback inject-button-press!]))
  (define id (seal but))

  (spawn
   (assert (button@ id))
   (enable/disable-handler but id)
   ;; NOTE - this assumes we are one level away from ground
   (on (message (inbound (button-press id)))
       (send! (button-press id))))

  id)

;; ID String (Listof String) -> ID
(define (spawn-choice #:parent parent
                      #:label label
                      #:choices choices)
  (define (inject-selection! c e)
    (send-ground-message (choice-selection id (send ch get-string-selection))))
  (define parent-component (seal-contents parent))
  (define ch (new choice%
                  [parent parent-component]
                  [label label]
                  [choices choices]
                  [callback inject-selection!]))
  (define id (seal ch))

  (spawn
   (field [selection (send ch get-string-selection)])
   (assert (choice@ id (selection)))

   (enable/disable-handler ch id)
   (on (message (inbound (choice-selection id $val)))
       (selection val)
       (send! (choice-selection id val)))
   (on (message (set-selection id $idx))
       (send ch set-selection idx)
       (selection (send ch get-string-selection))))

  id)

;; ID String Bool Nat -> ID
(define (spawn-gauge #:parent parent
                     #:label label
                     #:enabled [enabled? #t]
                     #:range [range 100])
  (define parent-component (seal-contents parent))
  (define g (new gauge%
                 [parent parent-component]
                 [label label]
                 [enabled enabled?]
                 [range range]))
  (define id (seal g))

  (spawn
   (assert (gauge@ id))
   (on (message (set-gauge-value id $v))
       (send g set-value v)))

  id)

;; ID String Nat Nat -> ID
(define (spawn-slider #:parent parent
                      #:label label
                      #:min-value [min-value 0]
                      #:max-value [max-value 100])
  (define (inject-slider-event! self evt)
    (send-ground-message (slider-update id (get))))

  (define parent-component (seal-contents parent))
  (define s (new slider%
                 [parent parent-component]
                 [label label]
                 [min-value min-value]
                 [max-value max-value]
                 [callback inject-slider-event!]))
  (define id (seal s))

  (define (get) (send s get-value))

  (spawn
   (field [current (get)])
   (assert (slider@ id (current)))
   (on (message (inbound (slider-update id $val)))
       (current val)
       (send! (slider-update id val))))

  id)

;; ID (U String #f) (Listof String) ... -> ID
(define (spawn-list-box #:parent parent
                        #:label label
                        #:choices choices
                        #:min-width [min-width #f]
                        #:min-height [min-height #f])
  (define (inject-list-box-selection! self evt)
    (send-ground-message (list-box-selection id (get))))
  (define parent-component (seal-contents parent))
  (define lb (new list-box%
                  [parent parent-component]
                  [label label]
                  [choices choices]
                  [min-width min-width]
                  [min-height min-height]
                  [callback inject-list-box-selection!]))
  (define id (seal lb))
  (define (get)
    (send lb get-selection))

  (spawn
   (field [selection (get)])
   (assert (list-box@ id (selection)))
   (on (message (inbound (list-box-selection id $val)))
       (selection val)
       (send! (list-box-selection id val)))
   (on (message (set-list-box-choices id $val))
       (send lb set val)
       (selection (get))))

  id)
