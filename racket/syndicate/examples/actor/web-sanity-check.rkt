#lang syndicate/actor
;; Simple demo of web driver. See web-demo.rkt for a more realistic example.

(require/activate syndicate/drivers/timer)
(require/activate syndicate/drivers/web)
(require net/url)

(define vh (web-virtual-host "http" ? 9090))

(define (sleep sec)
  (define timer-id (gensym 'sleep))
  (until (message (timer-expired timer-id _))
         (on-start (send! (set-timer timer-id (* sec 1000.0) 'relative)))))

(actor #:name 'server
       (react
        (field [counter 0])
        (assert vh)

        (on (message (web-request $id
                                  'inbound
                                  ($ req (web-request-header _ (web-resource vh `("ws" ())) _ _))
                                  _))
            (actor (react
                    (assert (web-response-websocket id))
                    (stop-when (retracted (observe (websocket-message id 'outbound _)))
                               (log-info "Connection dropped"))
                    (stop-when (message (websocket-message id 'inbound "quit"))
                               (log-info "Received quit command"))
                    (on (message (websocket-message id 'inbound $str))
                        (log-info "Got ~v" str)
                        (define u (string->url str))
                        (when (url-scheme u)
                          (let ((r (gensym 'client)))
                            (react (on-start
                                    (send! (web-request r
                                                        'outbound
                                                        (web-request-header 'get
                                                                            (url->resource u)
                                                                            '()
                                                                            '())
                                                        #"")))
                                   (stop-when (asserted (web-response-complete r $h $body))
                                              (log-info "Got headers back: ~v" h)
                                              (log-info "Got body back: ~v" body)))))
                        (send! (websocket-message id 'outbound str))))))

        (on (message (web-request $id
                                  'inbound
                                  (web-request-header 'get (web-resource vh `("slow" ())) _ _)
                                  _))
            (react (field [done? #f])
                   (stop-when (rising-edge (done?)))
                   (assert (web-response-chunked id
                                                 (web-response-header #:message #"Slowly"
                                                                      #:mime-type #"text/plain")))
                   (on (asserted (observe (web-response-chunk id _)))
                       ;;
                       ;; TODO: output-response-body/chunked in web-server's response.rkt
                       ;; doesn't flush each chunk as it appears. Should it?
                       ;;
                       ;; TODO: this kind of protocol pattern appears quite frequently. Perhaps
                       ;; we want a general-purpose *stream* protocol? For use by TCP,
                       ;; websockets, etc etc.
                       ;;
                       (send! (web-response-chunk id #"first\n"))
                       (sleep 2)
                       (send! (web-response-chunk id #"second\n"))
                       (sleep 2)
                       (send! (web-response-chunk id #"third\n"))
                       (sleep 2)
                       (done? #t))))

        (on (message (web-request $id
                                  'inbound
                                  (web-request-header 'get (web-resource vh `("foo" ,$path)) _ _)
                                  _))
            (define req-num (counter))
            (counter (+ (counter) 1))
            (send! (web-response-complete
                    id
                    (web-response-header #:mime-type #"text/plain")
                    (string->bytes/utf-8
                     (format "Hi there. Your path was ~v, and this is request ~a"
                             path
                             req-num)))))))
