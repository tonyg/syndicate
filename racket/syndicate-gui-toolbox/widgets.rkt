#lang syndicate

(provide spawn-frame
         spawn-horizontal-pane
         spawn-text-field
         spawn-button
         spawn-choice
         (struct-out frame@)
         (struct-out show-frame)
         (struct-out horizontal-pane@)
         (struct-out text-field@)
         (struct-out set-text-field)
         (struct-out button@)
         (struct-out button-press)
         (struct-out set-text-field-background)
         (struct-out text-field-update)
         (struct-out choice@)
         (struct-out choice-selection)
         (struct-out set-selection)
         (struct-out enable))

(require (only-in racket/class
                  new
                  send
                  make-object))
(require racket/gui/base)

;; an ID is a (Sealof Any)

(message-struct enable (id val))

(assertion-struct frame@ (id))
(message-struct show-frame (id value))

(assertion-struct horizontal-pane@ (id))

(assertion-struct text-field@ (id value))
(message-struct set-text-field (id value))
(message-struct set-text-field-background (id color))
(message-struct text-field-update (id value))

(assertion-struct button@ (id))
(message-struct button-press (id))

(assertion-struct choice@ (id selection))
(message-struct choice-selection (id val))
(message-struct set-selection (id idx))

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
(define (spawn-horizontal-pane #:parent parent)
  (define parent-component (seal-contents parent))
  (define pane (new horizontal-pane% [parent parent-component]))
  (define id (seal pane))

  (spawn
   (assert (horizontal-pane@ id)))

  id)

; ID String String Bool Nat -> ID
(define (spawn-text-field #:parent parent
                          #:label label
                          #:init-value init
                          #:enabled [enabled? #t]
                          #:min-width [min-width 1])
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
