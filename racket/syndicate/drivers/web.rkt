#lang syndicate/actor
;; More general web driver: supports normal HTTP as well as websockets.

(provide (struct-out web-virtual-host)
         (struct-out web-resource)
         url->resource

         (struct-out web-request)
         (struct-out web-request-header)

         (rename-out [web-response-header <web-response-header>])
         (struct-out/defaults [make-web-response-header web-response-header])
         (struct-out web-response-complete)
         (struct-out web-response-chunked)
         (rename-out [web-response-websocket <web-response-websocket>])
         (struct-out/defaults [make-web-response-websocket web-response-websocket])

         (struct-out web-response-chunk)
         (struct-out websocket-message)

         web-request-incoming
         web-request-get
         websocket-connection-closed
         websocket-message-recv
         websocket-message-send!
         web-respond/bytes!
         web-respond/xexpr!

         spawn-web-driver)

(require net/url)
(require net/rfc6455)
(require net/rfc6455/conn-api)
(require net/rfc6455/dispatcher)
(require net/http-client)
(require racket/exn)
(require racket/tcp)
(require racket/set)
(require racket/async-channel)
(require (only-in racket/bytes bytes-join))
(require (only-in racket/list flatten))
(require (only-in racket/port port->bytes))
(require web-server/http/bindings)
(require web-server/http/request)
(require web-server/http/request-structs)
(require web-server/http/response)
(require web-server/http/response-structs)
(require web-server/private/connection-manager)
(require (only-in web-server/private/util lowercase-symbol!))
(require web-server/dispatchers/dispatch)
(require struct-defaults)
(require xml)

(require/activate "timer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct web-virtual-host (scheme name port) #:prefab)
(struct web-resource (virtual-host path) #:prefab)

(struct web-request (id direction header* body) #:prefab)
(struct web-request-header (method resource headers query) #:prefab)

(struct web-response-header (code message last-modified-seconds mime-type headers) #:prefab)
(struct web-response-complete (id header body) #:prefab)
(struct web-response-chunked (id header) #:prefab)
(struct web-response-websocket (id headers) #:prefab)

(struct web-response-chunk (id bytes) #:prefab)
(struct websocket-message (id direction body) #:prefab)

(begin-for-declarations
  (define-struct-defaults make-web-response-header web-response-header
    (#:code [web-response-header-code 200]
     #:message [web-response-header-message #"OK"]
     #:last-modified-seconds [web-response-header-last-modified-seconds (current-seconds)]
     #:mime-type [web-response-header-mime-type #"text/html"]
     #:headers [web-response-header-headers '()]))
  (define-struct-defaults make-web-response-websocket web-response-websocket
    (#:headers [web-response-websocket-headers '()])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground-level communication messages

(struct web-raw-request (id port connection req control-ch) #:prefab)
(struct web-raw-client-conn (id connection) #:prefab)
(struct web-incoming-message (id message) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event-expander web-request-incoming
  (syntax-rules ()
    [(_ (id req) vh method path)
     (message (web-request ($ id _)
                           'inbound
                           ($ req (web-request-header method (web-resource vh `path) _ _))
                           _))]))

(define-event-expander web-request-get
  (syntax-rules ()
    [(_ (id req) vh path)
     (web-request-incoming (id req) vh 'get path)]))

(define-event-expander websocket-connection-closed
  (syntax-rules ()
    [(_ id)
     (retracted (observe (websocket-message id 'outbound _)))]))

(define-event-expander websocket-message-recv
  (syntax-rules ()
    [(_ id str)
     (message (websocket-message id 'inbound str))]))

(define (websocket-message-send! id str)
  (send! (websocket-message id 'outbound str)))

(define (web-respond/bytes! id #:header [header (make-web-response-header)] body-bytes)
  (send! (web-response-complete id header body-bytes)))

(define (web-respond/xexpr! id
                            #:header [header (make-web-response-header)]
                            #:preamble [preamble #"<!DOCTYPE html>"]
                            body-xexpr)
  (web-respond/bytes! id #:header header
                      (bytes-append preamble
                                    (string->bytes/utf-8 (xexpr->string body-xexpr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define web-server-max-waiting (make-parameter 511)) ;; sockets
(define web-server-connection-manager (make-parameter #f))
(define web-server-initial-connection-timeout (make-parameter 30)) ;; seconds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (url->resource u)
  (web-resource (web-virtual-host (url-scheme u)
                                  (url-host u)
                                  (url-port u))
                (format-url-path u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-web-driver)
  (actor #:name 'web-server-manager
         (react
          (during/actor (web-virtual-host "http" _ $port)
                        #:name (list 'web-server port)
                        (setup-web-server "http"
                                          (or (web-server-connection-manager)
                                              (start-connection-manager))
                                          port))))
  (actor #:name 'web-client-manager
         (react
          (on (message (web-request $id 'outbound $req $body))
              (actor #:name (list 'web-client id)
                     (do-client-request id req body))))))

(define (setup-web-server scheme cm port)
  (define listener (tcp-listen port (web-server-max-waiting) #t))
  (define listener-control (make-channel))
  (thread (lambda ()
            (let loop ()
              (sync (handle-evt (tcp-accept-evt listener)
                                (lambda (ports)
                                  (handle-incoming-connection port cm ports)
                                  (loop)))
                    (handle-evt listener-control
                                (match-lambda
                                  ['quit (void)]))))))

  (on-stop (channel-put listener-control 'quit))

  (on (message (web-raw-request $id port $conn $lowlevel-req $control-ch) #:meta-level 1)
      (define web-req (web-request id
                                   'inbound
                                   (web-request-header
                                    (string->symbol (string-downcase
                                                     (bytes->string/latin-1
                                                      (request-method lowlevel-req))))
                                    (web-resource (req->virtual-host scheme lowlevel-req port)
                                                  (format-url-path (request-uri lowlevel-req)))
                                    (request-headers lowlevel-req)
                                    (url-query (request-uri lowlevel-req)))
                                   (request-post-data/raw lowlevel-req)))
      (actor #:name (list 'web-req id)
             (react (on-start (send! (set-timer (list 'web-req id) 100 'relative))
                              (send! web-req))
                    (stop-when (message (timer-expired (list 'web-req id) _))
                               (do-response-complete control-ch
                                                     id
                                                     (make-web-response-header
                                                      #:code 404
                                                      #:message #"Not found")
                                                     '()))
                    (stop-when (message (web-response-complete id $rh $body))
                               (do-response-complete control-ch id rh body))
                    (stop-when (asserted (web-response-chunked id $rh))
                               (do-response-chunked control-ch id rh))
                    (stop-when (asserted (web-response-websocket id $headers))
                               (do-response-websocket control-ch id headers))))))

(define (do-response-complete control-ch id rh constree-of-bytes)
  (match-define (web-response-header code resp-message last-modified-seconds mime-type headers) rh)
  (channel-put control-ch
               (list 'response
                     (response/full code
                                    resp-message
                                    last-modified-seconds
                                    mime-type
                                    (build-headers headers)
                                    (flatten constree-of-bytes)))))

(define (do-response-chunked control-ch id rh)
  (match-define (web-response-header code resp-message last-modified-seconds mime-type headers) rh)
  (define stream-ch (make-async-channel))
  (react (stop-when (retracted (web-response-chunked id rh)))
         (on-stop (async-channel-put stream-ch #f))
         (on (message (web-response-chunk id $chunk))
             (async-channel-put stream-ch (flatten chunk)))
         (on-start (channel-put control-ch
                                (list 'response
                                      (response code
                                                resp-message
                                                last-modified-seconds
                                                mime-type
                                                (build-headers headers)
                                                (lambda (output-port)
                                                  (let loop ()
                                                    (match (async-channel-get stream-ch)
                                                      [#f
                                                       (void)]
                                                      [bss (for [(bs (in-list bss))]
                                                             (write-bytes bs
                                                                          output-port))
                                                           (loop)])))))))))

(define (do-response-websocket control-ch id headers)
  (define ws-ch (make-channel))
  (react (stop-when (retracted (web-response-websocket id headers)))
         (on-start (channel-put control-ch (list 'websocket headers ws-ch)))
         (run-websocket-connection id ws-ch)))

(define (run-websocket-connection id ws-ch)
  (on-stop (channel-put ws-ch 'quit))
  (on (message (websocket-message id 'outbound $body))
      (channel-put ws-ch (list 'send body)))
  (stop-when (message (web-incoming-message id (? eof-object? _)) #:meta-level 1))
  (on (message (web-incoming-message id $body) #:meta-level 1)
      (unless (eof-object? body) (send! (websocket-message id 'inbound body)))))

(define (req->virtual-host scheme r port)
  (cond [(assq 'host (request-headers r)) =>
         (lambda (h)
           (match (cdr h)
             [(regexp #px"(.*):(\\d+)" (list _ host port))
              (web-virtual-host scheme host (string->number port))]
             [host
              (web-virtual-host scheme host port)]))]
        [else
         (web-virtual-host scheme #f port)]))

(define (format-url-path u)
  (define elements (for/list [(p (in-list (url-path u)))]
                     (match-define (path/param path-element params) p)
                     (list* path-element params)))
  (foldr (lambda (e acc) (append e (list acc))) '() elements))

(define (build-headers hs)
  (for/list ((h (in-list hs)))
    (header (string->bytes/utf-8 (symbol->string (car h)))
            (string->bytes/utf-8 (cdr h)))))

(define (build-http-client-headers hs)
  (for/list ((h (in-list hs)))
    (format "~a: ~a" (car h) (cdr h))))

(define (handle-incoming-connection listen-port cm connection-ports)
  (thread
   (lambda ()
     (match-define (list i o) connection-ports)
     ;; Deliberately construct an empty custodian for the connection. Killing the connection
     ;; abruptly can cause deadlocks since the connection thread communicates with Syndicate
     ;; via synchronous channels.
     (define conn
       (new-connection cm (web-server-initial-connection-timeout) i o (make-custodian) #f))
     (define control-ch (make-channel))
     (let do-request ()
       (define-values (req initial-headers) ;; TODO initial-headers?!?!
         (with-handlers ([exn:fail? (lambda (e) (values #f #f))])
           (read-request conn listen-port tcp-addresses)))
       (when req
         (define id (gensym 'web))
         (send-ground-message (web-raw-request id listen-port conn req control-ch))
         (sync (handle-evt control-ch
                           (match-lambda
                             [(list 'websocket reply-headers ws-ch)
                              (with-handlers ((exn:dispatcher?
                                               (lambda (_e) (bad-request conn req))))
                                ((make-general-websockets-dispatcher
                                  (websocket-connection-main id ws-ch)
                                  (lambda _args (values reply-headers (void))))
                                 conn req))]
                             [(list 'response resp)
                              (output-response/method conn resp (request-method req))])))
         (do-request))))))

;; D-:  uck barf
;; TODO: something to fix this :-/
(define (exn:fail:port-is-closed? e)
  (and (exn:fail? e)
       (regexp-match #px"port is closed" (exn-message e))))

(define ((websocket-connection-main id ws-ch) wsc _ws-connection-state)
  (let loop ()
    (sync (handle-evt wsc
                      (lambda _args
                        (define msg
                          (with-handlers ([exn:fail:network? (lambda (e) eof)]
                                          [exn:fail:port-is-closed? (lambda (e) eof)]
                                          [exn:fail? (lambda (e)
                                                       (log-error "Unexpected ws-recv error: ~a"
                                                                  (exn->string e))
                                                       eof)])
                            (ws-recv wsc #:payload-type 'text)))
                        (send-ground-message (web-incoming-message id msg))
                        (loop)))
          (handle-evt ws-ch
                      (match-lambda
                        ['quit
                         (void)]
                        [(list 'send m)
                         (with-handlers [(exn:fail:port-is-closed?
                                          (lambda (e)
                                            (ws-close! wsc)))]
                           (ws-send! wsc m))
                         (loop)]))))
  (ws-close! wsc))

(define (bad-request conn req)
  (output-response/method conn
                          (response/full 400
                                         #"Bad request"
                                         (current-seconds)
                                         #"text/plain"
                                         (list)
                                         (list))
                          (request-method req)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-client-request id req body)
  (react
   (stop-when (asserted (observe (web-response-websocket id _)))
              (do-request-websocket id req))
   (stop-when (asserted (observe (web-response-complete id _ _)))
              (do-request-complete id req body))
   (stop-when (asserted (observe (web-response-chunked id _)))
              (do-request-chunked id req body))))

(define (analyze-outbound-request req)
  (match-define (web-request-header method
                                    (web-resource (web-virtual-host scheme host port) path)
                                    headers
                                    query)
    req)
  (values host
          (or port (match scheme
                     ["http" 80]
                     ["https" 443]
                     [_ #f]))
          method
          (url->string (url scheme
                            #f
                            host
                            port
                            #t
                            (let loop ((p path))
                              (match p
                                ['() '()]
                                [(list d par ... rest)
                                 (cons (path/param d par) (loop rest))]))
                            query
                            #f))
          headers))

(define (do-request-websocket id req)
  (define-values (_host server-port method urlstr headers) (analyze-outbound-request req))
  (define control-ch (make-channel))
  (if (not server-port)
      (send-ground-message (web-raw-client-conn id #f))
      (thread
       (lambda ()
         (log-info "Connecting to ~a ~a" urlstr (current-inexact-milliseconds))
         (define c (with-handlers [(exn? values)]
                     (ws-connect (string->url urlstr) #:headers headers)))
         (when (exn? c)
           (log-info "Connection to ~a failed: ~a" urlstr (exn->string c)))
         (send-ground-message (web-raw-client-conn id c))
         (when (not (exn? c))
           (log-info "Connected to ~a ~a" url (current-inexact-milliseconds))
           ((websocket-connection-main id control-ch) c (void))))))
  (react
   (stop-when (message (web-raw-client-conn id $c) #:meta-level 1)
              (react (stop-when (retracted (observe (web-response-websocket id _))))
                     (if (ws-conn? c)
                         (begin (assert (web-response-websocket id (ws-conn-headers c)))
                                (run-websocket-connection id control-ch))
                         (assert (web-response-websocket id #f)))))))

(define (do-request-complete id req body)
  (define-values (host server-port method urlstr headers) (analyze-outbound-request req))
  (thread
   (lambda ()
     (define response
       (with-handlers [(exn? values)]
         (when (not server-port)
           (error 'http-sendrecv "No server port specified"))
         (define-values (first-line header-lines body-port)
           (http-sendrecv host
                          urlstr
                          #:headers (build-http-client-headers headers)
                          #:port server-port
                          #:method (string-upcase (symbol->string method))
                          #:data (bytes-join (flatten body) #"")))
         (match first-line
           [(regexp #px"\\S+\\s(\\d+)\\s(.*)" (list _ codebs msgbs))
            (define code (string->number (bytes->string/latin-1 codebs)))
            (define msg (bytes->string/utf-8 msgbs))
            (define response-headers
              (for/list ((h (in-list (read-headers (open-input-bytes
                                                    (bytes-join header-lines #"\r\n"))))))
                (match-define (header k v) h)
                (cons (lowercase-symbol! (bytes->string/utf-8 k))
                      (bytes->string/utf-8 v))))
            (define response-body (port->bytes body-port))
            (web-response-complete id
                                   (web-response-header code
                                                        msg
                                                        #f ;; TODO: fill in from response-headers
                                                        (cond [(assq 'content-type response-headers)
                                                               => cdr]
                                                              [else #f])
                                                        response-headers)
                                   response-body)]
           [_
            (error 'http-sendrecv "Bad first line: ~v" first-line)])))
     (send-ground-message (web-raw-client-conn id response))))
  (react
   (stop-when (message (web-raw-client-conn id $r) #:meta-level 1)
              (react (stop-when (retracted (observe (web-response-complete id _ _))))
                     (if (exn? r)
                         (assert (web-response-websocket id #f #f))
                         (begin (assert r)))))))

(define (do-request-chunked id req body)
  (log-error "do-request-chunked: unimplemented")
  (react (stop-when (retracted (observe (web-response-chunked id _))))
         (assert (web-response-chunked id #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-web-driver)
