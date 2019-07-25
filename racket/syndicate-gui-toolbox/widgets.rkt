#lang syndicate

(provide gui-eventspace
         gui-callback
         qc
         spawn-frame
         spawn-horizontal-pane
         spawn-horizontal-panel
         spawn-vertical-pane
         spawn-text-field
         spawn-button
         spawn-choice
         spawn-gauge
         spawn-slider
         spawn-list-box
         spawn-dialog
         (struct-out frame@)
         (struct-out show)
         (struct-out horizontal-pane@)
         (struct-out horizontal-panel@)
         (struct-out vertical-pane@)
         (struct-out text-field@)
         (struct-out set-text-field)
         (struct-out button@)
         (struct-out button-press)
         (struct-out set-text-field-background)
         (struct-out text-field-update)
         (struct-out text-field-enter)
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
         (struct-out set-list-box-choices)
         (struct-out popup-menu)
         (struct-out no-popdown-selected)
         (struct-out popdown-item-selected)
         (struct-out dialog@))

(require (only-in racket/class
                  new
                  send
                  make-object))
(require racket/gui/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eventspace Shennanigans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gui-eventspace (make-parameter (make-eventspace)))

(define (gui-callback thnk)
  (parameterize ([current-eventspace (gui-eventspace)])
    (queue-callback thnk)))

(define-syntax-rule (qc expr ...)
  (gui-callback (lambda () expr ...)))


;; an ID is a (Sealof Any)
;; an Alignment is a (List (U 'left 'center 'right) (U 'top 'center 'bottom))

(message-struct enable (id val))

(assertion-struct frame@ (id))
(message-struct show (id value))
(message-struct popup-menu (parent-id id title x y items))
(message-struct no-popdown-selected (id))
(message-struct popdown-item-selected (id item))

(assertion-struct horizontal-pane@ (id))
(assertion-struct vertical-pane@ (id))
(assertion-struct horizontal-panel@ (id))

(assertion-struct text-field@ (id value))
(message-struct set-text-field (id value))
(message-struct set-text-field-background (id color))
(message-struct text-field-update (id value))
(message-struct text-field-enter (id value))

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

(assertion-struct dialog@ (id))

(define (enable/disable-handler self my-id)
  (on (message (enable my-id $val))
      (qc (send self enable val))))

;; String -> ID
(define (spawn-frame #:label label
                     #:width [width #f]
                     #:height [height #f])
  (define frame
    (parameterize ((current-eventspace (gui-eventspace)))
      (new frame%
           [label label]
           [width width]
           [height height])))
  (define id (seal frame))

  (define ((on-popdown! pid) self evt)
    (when (eq? (send evt get-event-type) 'menu-popdown-none)
      (send-ground-message (no-popdown-selected pid))))
  (define ((popdown-item! pid i) . _x)
    (send-ground-message (popdown-item-selected pid i)))

  (spawn
   (assert (frame@ id))
   (on (message (show id $val))
       (qc (send frame show val)))
   (on (message (popup-menu id $pid $title $x $y $items))
       (define pm (new popup-menu% [title title] [popdown-callback (on-popdown! pid)]))
       (for ([i (in-list items)])
         (new menu-item% [parent pm] [label i] [callback (popdown-item! pid i)]))
       (qc (send frame popup-menu pm x y))
       (react (stop-when (message (inbound (no-popdown-selected pid))) (send! (no-popdown-selected pid)))
              (stop-when (message (inbound (popdown-item-selected pid $i))) (send! (popdown-item-selected pid i))))))
  id)

;; ID ... -> ID
(define (spawn-horizontal-pane #:parent parent
                               #:border [border 0]
                               #:min-height [min-height #f]
                               #:alignment [alignment '(left center)])
  (define parent-component (seal-contents parent))
  (define pane (new horizontal-pane%
                    [parent parent-component]
                    [border border]
                    [min-height min-height]
                    [alignment alignment]))
  (define id (seal pane))

  (spawn
   (assert (horizontal-pane@ id)))

  id)

;; ID ... -> ID
(define (spawn-horizontal-panel #:parent parent
                                #:border [border 0]
                                #:min-height [min-height #f]
                                #:alignment [alignment '(left center)])
  (define parent-component (seal-contents parent))
  (define panel (new horizontal-panel%
                     [parent parent-component]
                     [border border]
                     [min-height min-height]
                     [alignment alignment]))
  (define id (seal panel))

  (spawn
   (assert (horizontal-panel@ id)))

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
                          #:min-width [min-width #f]
                          #:min-height [min-height #f])
  (define parent-component (seal-contents parent))

  (define (inject-text-field-update! _ evt)
    (case (send evt get-event-type)
      [(text-field)
       (send-ground-message (text-field-update id (send tf get-value)))]
      [(text-field-enter)
       (send-ground-message (text-field-enter id (send tf get-value)))]))

  (define tf (new text-field%
                  [parent parent-component]
                  [label label]
                  [init-value init]
                  [enabled enabled?]
                  [min-width min-width]
                  [min-height min-height]
                  [callback inject-text-field-update!]))
  (define id (seal tf))

  (spawn
   (field [val (send tf get-value)])
   (assert (text-field@ id (val)))
   (enable/disable-handler tf id)
   (on (message (set-text-field id $value))
       (qc (send tf set-value value))
       (val value))
   (on (message (set-text-field-background id $color))
       (define c (make-object color% color))
       (qc (send tf set-field-background c)))
   (on (message (inbound (text-field-update id $value)))
       (val value)
       (send! (text-field-update id value)))
   (on (message (inbound (text-field-enter id $value)))
       (val value)
       (send! (text-field-enter id value))))

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
       (qc (send ch set-selection idx))
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
       (qc (send g set-value v))))

  id)

;; ID String Nat Nat -> ID
(define (spawn-slider #:parent parent
                      #:label label
                      #:min-value min-value
                      #:max-value max-value
                      #:init-value [init-value min-value])
  (define (inject-slider-event! self evt)
    (send-ground-message (slider-update id (get))))

  (define parent-component (seal-contents parent))
  (define s (new slider%
                 [parent parent-component]
                 [label label]
                 [min-value min-value]
                 [max-value max-value]
                 [init-value init-value]
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
       (qc (send lb set val))
       (selection (get))))

  id)

(define (spawn-dialog #:label label
                      #:parent [parent #f]
                      #:style [style null])
  (define parent-component (and parent (seal-contents parent)))
  (define evt-spc (if parent-component
                      (send parent-component get-eventspace)
                      (make-eventspace) #;(gui-eventspace)))
  (define d (parameterize ((current-eventspace evt-spc))
              (new dialog%
                   [label label]
                   [parent parent-component]
                   [style style])))
  (define id (seal d))

  (spawn
   (assert (dialog@ id))

   (on (message (show id $show?))
       (qc (send d show show?))
       (unless show? (stop-current-facet))))

  id)
