#lang syndicate/actor
;; Driver for Racket's GUI.

(require racket/class)
(require racket/gui)

(provide (struct-out widget)
         (struct-out widget-prop)
         (struct-out widget-object)
         (struct-out widget-event)
         assert-widget)

(module+ implementation-details
  (provide (struct-out widget-query)
           (struct-out frames-present)
           (struct-out frame-closing)))

(struct widget (id type) #:prefab) ;; assertion
(struct widget-prop (id key value) #:prefab) ;; assertion
(struct widget-object (id object) #:prefab) ;; assertion
(struct widget-event (id class type event) #:prefab) ;; message

(struct widget-query (object key ch) #:prefab) ;; message
(struct frames-present () #:prefab) ;; assertion
(struct frame-closing (object) #:prefab) ;; message

(define-syntax-rule (assert-widget id type [prop init] ...)
  (begin (assert (widget id type))
         (assert (widget-prop id 'prop init)) ...))

(define *widget-types* (make-hasheq))

(define (define-widget-type! type proc)
  (hash-set! *widget-types* type proc))

(define-syntax-rule (define-widget-type (type id) body ...)
  (define-widget-type! 'type
    (lambda (id _type)
      body ...)))

(define-syntax defprop
  (syntax-rules ()
    [(_ w id prop-key #:widget setter-expr)
     (begin (define/query-value -wid- #f (widget-prop id 'prop-key $wid-value) wid-value)
            (define/query-value prop-key #f (widget-object (-wid-) $o) o
              #:on-add (let ((prop-key (prop-key)))
                         (log-info "Invoking setter for ~a widget-prop ~a: ~a" id 'prop-key prop-key)
                         setter-expr)))]
    [(_ w id prop-key default setter-expr)
     (define/query-value prop-key default (widget-prop id 'prop-key $prop-value) prop-value
       #:on-add (let ((prop-key (prop-key)))
                  (log-info "Invoking setter for ~a prop ~a: ~a" id 'prop-key prop-key)
                  setter-expr))]))

(define-syntax-rule (with-widget-props w id [[defprop-items ...] ...] body ...)
  (begin
    (defprop w id defprop-items ...) ...
    (on-start (flush!) ;; allow queries to have a go at the dataspace
              (react body ...))))

(define-syntax-rule (maybe-send receiver selector args ...)
  (and receiver
       (send receiver selector args ...)))

(define syndicate-frame%
  (class* frame% ()
    (super-new)
    (define/augment (can-close?)
      (define ch (make-channel))
      (send-ground-message (widget-query this 'can-close? ch))
      (define answers (channel-get ch))
      (not (set-member? answers #f)))
    (define/augment (on-close)
      (send-ground-message (frame-closing this)))))

(define-widget-type (frame id)
  (define outermost-facet (current-facet-id))
  (define w #f)
  (on-stop (maybe-send w show #f))
  (with-widget-props w id [[label "" (maybe-send w set-label label)]
                           [parent #:widget (maybe-send w reparent parent)]]
    (parameterize ((current-eventspace (make-eventspace)))
      (set! w (new syndicate-frame%
                   [label (label)]
                   [parent (parent)]
                   ))
      (send w show #t))
    (on (message (inbound (frame-closing w)))
        (stop-facet outermost-facet))
    (assert (frames-present))
    (assert (widget-object id w))))

(define-syntax-rule (during-parent [id parent-id-var parent-var] body ...)
  (during (widget-prop id 'parent $parent-id)
    (during (widget-object parent-id $parent)
      (log-info "~a --parent--> ~a" id parent-id)
      (let ((parent-id-var parent-id)
            (parent-var parent))
        body ...))))

(define (control-event-callback id)
  (lambda (_widget e)
    (send-ground-message (widget-event id 'control (send e get-event-type) e))))

(define-syntax-rule (on-stop-delete-child w)
  (on-stop (maybe-send (maybe-send w get-parent) delete-child w)))

(define-widget-type (button id)
  (during-parent [id parent-id parent]
    (define w #f)
    (on-stop-delete-child w)
    (with-widget-props w id [[label "" (maybe-send w set-label label)]
                             [enabled #t (maybe-send w enable enabled)]]
      (set! w (new button%
                   [label (label)]
                   [parent parent]
                   [enabled (enabled)]
                   [callback (control-event-callback id)]))
      (assert (widget-object id w)))))

(define-widget-type (message id)
  (during-parent [id parent-id parent]
    (define w #f)
    (on-stop-delete-child w)
    (with-widget-props w id [[label "" (maybe-send w set-label label)]]
      (set! w (new message%
                   [label (label)]
                   [parent parent]))
      (assert (widget-object id w)))))

(define-syntax-rule (on-stop-delete-menu-item w)
  (on-stop (maybe-send w delete)))

(define (get-menu-eventspace x)
  (cond
    [(is-a? x menu%) (get-menu-eventspace (send x get-parent))]
    [(is-a? x menu-bar%) (get-menu-eventspace (send x get-frame))]
    [(is-a? x frame%) (send x get-eventspace)]))

(define-widget-type (menu-bar id)
  (during-parent [id parent-id parent]
    (define w #f)
    ;; (on-stop-delete w) ;; It turns out there is no way to remove a menu-bar% from a frame% !
    (with-widget-props w id []
      (parameterize ((current-eventspace (get-menu-eventspace parent)))
        (set! w (new menu-bar%
                     [parent parent])))
      (assert (widget-object id w)))))

(define-widget-type (menu id)
  (during-parent [id parent-id parent]
    (define w #f)
    (on-stop-delete-menu-item w)
    (with-widget-props w id [[label "" (maybe-send w set-label label)]]
      (parameterize ((current-eventspace (get-menu-eventspace parent)))
        (set! w (new menu%
                     [label (label)]
                     [parent parent])))
      (assert (widget-object id w)))))

(define-widget-type (menu-item id)
  (during-parent [id parent-id parent]
    (define w #f)
    (on-stop-delete-menu-item w)
    (with-widget-props w id [[label "" (maybe-send w set-label label)]]
      (parameterize ((current-eventspace (get-menu-eventspace parent)))
        (set! w (new menu-item%
                     [label (label)]
                     [parent parent]
                     [callback (control-event-callback id)])))
      (assert (widget-object id w)))))

;;---------------------------------------------------------------------------

(spawn #:name 'widget-server
       (during/spawn (widget $id $type)
                     #:name (widget id type)
                     (match (hash-ref *widget-types* type #f)
                       [#f (error 'widget "Invalid widget type ~a (ID ~a, parent-id ~a)" type id)]
                       [proc (proc id type)]))
       (during (frames-present)
         (on (message (inbound (widget-event $id $class $type $event)))
             (send! (widget-event id class type event)))
         (on (message (inbound (widget-query $w $prop-key $ch)))
             (react (define f (current-facet-id))
                    (during (widget-object $id w)
                      (on-start
                       (channel-put ch (immediate-query (query-set (widget-prop id prop-key $v) v)))
                       (stop-facet f)))))))
