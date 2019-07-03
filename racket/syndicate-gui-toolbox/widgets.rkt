#lang syndicate

(provide spawn-frame
         spawn-horizontal-pane
         spawn-text-field
         spawn-button
         (struct-out frame@)
         (struct-out show-frame)
         (struct-out horizontal-pane@)
         (struct-out text-field@)
         (struct-out set-text-field)
         (struct-out button@)
         (struct-out button-press))

(require (only-in racket/class
                  new
                  send))
(require racket/gui/base)

;; an ID is a (Sealof Any)

(assertion-struct frame@ (id))
(message-struct show-frame (id value))

(assertion-struct horizontal-pane@ (id))

(assertion-struct text-field@ (id))
(message-struct set-text-field (id value))

(assertion-struct button@ (id))
(message-struct button-press (id))

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
                          #:enabled enabled?
                          #:min-width min-width)
  (define parent-component (seal-contents parent))
  (define tf (new text-field%
                  [parent parent-component]
                  [label label]
                  [init-value init]
                  [enabled enabled?]
                  [min-width min-width]))
  (define id (seal tf))

  (spawn
   (assert (text-field@ id))
   (on (message (set-text-field id $value))
       (send tf set-value value)))

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
   ;; NOTE - this assumes we are one level away from ground
   (on (message (inbound (button-press id)))
       (send! (button-press id))))

  id)
