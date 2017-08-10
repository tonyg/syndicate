#lang syndicate
;; Small test program for the gui.rkt driver

(require/activate syndicate/drivers/gui)

(spawn #:name 'demo-w
       (field [can-close? #f])

       (assert-widget 'w 'frame
                      [label "Hello Syndicate"]
                      [can-close? (can-close?)])
       (on (asserted (observe (widget-prop 'w 'can-close? _)))
           ;; TODO: Reconsider this. Should it be possible to lazily
           ;; compute whether or not `can-close?`, when we detect that
           ;; someone cares? At present, by the time we find out
           ;; someone cares, it's too late to influence their
           ;; decision.
           (log-info "Oo err, someone cares whether the window can close")
           (can-close? #t))

       (assert-widget 'm 'message
                      [parent 'w]
                      [label (format "Window will ~aclose if close icon clicked"
                                     (if (can-close?) "" "not "))])

       (assert-widget 'w-menu 'menu-bar [parent 'w])
       (assert-widget 'w-file 'menu [parent 'w-menu] [label "File"])
       (assert-widget 'w-exit 'menu-item [parent 'w-file] [label "Exit"])
       (assert-widget 'w-hello 'menu-item [parent 'w-file] [label "Hello!"])

       (stop-when (message (widget-event 'w-exit _ _ _)))
       (on (message (widget-event 'w-hello _ _ _))
           (log-info "Hello!")))

(spawn #:name 'demo-b
       (field [enabled #f])

       (assert-widget 'b 'button
                      [parent 'w]
                      [label "Click me"]
                      [enabled (enabled)])
       (stop-when (message (widget-event 'b $c $t $e))
           (log-info "Event: ~v/~v/~v" c t e))

       (assert-widget 'b2 'button
                      [parent 'w]
                      [label "Enable the other one"])
       (on (message (widget-event 'b2 _ _ _))
           (enabled #t)))
