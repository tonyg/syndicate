#lang syndicate

(provide (struct-out websocket-remote-client)
	 (struct-out websocket-local-server)
	 (struct-out websocket-local-client)
	 (struct-out websocket-remote-server)
	 (struct-out websocket-ssl-options)
	 (struct-out websocket-message)
         (struct-out websocket-peer-details)
	 spawn-websocket-driver
         any-websocket-remote-client)

(require racket/exn)
(require net/rfc6455)
(require (only-in net/rfc6455/conn-api
                  ws-conn-base-ip
                  ws-conn-peer-addresses
                  ws-conn-host+port
                  ws-conn-path))
(require syndicate/demand-matcher)
(require syndicate/protocol/advertise)

(require racket/unit)
(require racket/tcp)
(require net/tcp-sig)
(require net/tcp-unit)
(require net/ssl-tcp-unit)
(require net/url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct websocket-remote-client (id request-host request-port request-path) #:prefab)
(struct websocket-local-server (port ssl-options) #:prefab)
(struct websocket-local-client (id) #:prefab)
(struct websocket-remote-server (url) #:prefab)
(struct websocket-ssl-options (cert-file key-file) #:prefab)
(struct websocket-message (from to body) #:prefab)

(struct websocket-peer-details
  (local-addr remote-addr local-ip local-port remote-ip remote-port)
  #:prefab)

(define any-websocket-remote-client (websocket-remote-client ? ? ? ?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground-level communication messages

(struct websocket-connection (id local-addr remote-addr connection control-ch) #:prefab)
(struct websocket-incoming-message (id message) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver

(define (spawn-websocket-driver)
  (define inbound-listener-message-pat (websocket-message ? (?! (websocket-local-server ? ?)) ?))
  (define outbound-conn-message-pat (websocket-message (?! (websocket-local-client ?))
                                                       (?! (websocket-remote-server ?))
                                                       ?))
  (list (spawn-demand-matcher (advertise (observe inbound-listener-message-pat))
                              (advertise (advertise inbound-listener-message-pat))
                              spawn-websocket-listener
                              #:name 'drivers/websocket:dm:listen)
        (spawn-demand-matcher (advertise outbound-conn-message-pat)
                              (observe outbound-conn-message-pat)
                              spawn-websocket-connection
                              (lambda (local-addr remote-addr)
                                (log-debug "Outbound websocket connection closed: ~v -> ~v"
                                           local-addr
                                           remote-addr))
                              #:name 'drivers/websocket:dm:connect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener

(struct listener-state (shutdown-procedure server-addr) #:transparent)

(define (websocket-listener e state)
  (match e
    [(? patch/removed? p)
     ((listener-state-shutdown-procedure state))
     (quit)]
    [(message (inbound (websocket-connection id local-addr remote-addr c control-ch)))
     (transition state (spawn-connection local-addr remote-addr id c control-ch))]
    [_ #f]))

(define ((connection-handler server-addr) c dummy-state)
  (define control-ch (make-channel))
  (define id (gensym 'ws))
  (define-values (client-host client-port) (ws-conn-host+port c))
  (send-ground-message
   (websocket-connection id
                         server-addr
                         (websocket-remote-client id
                                                  client-host
                                                  client-port
                                                  (ws-conn-path c))
                         c
                         control-ch))
  (connection-thread-loop control-ch c id))

(define (connection-thread-loop control-ch c id)
  (define c-input-port (ws-conn-base-ip c))
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt c-input-port
			  (lambda (dummy)
			    (define msg
			      (with-handlers ([exn:fail:network? (lambda (e) eof)]
                                              [exn:fail? (lambda (e)
                                                           (log-error "Unexpected ws-recv error: ~a"
                                                                      (exn->string e))
                                                           eof)])
				(ws-recv c #:payload-type 'text)))
			    (send-ground-message (websocket-incoming-message id msg))
			    (loop (or blocked? (eof-object? msg))))))))
  (ws-close! c))

(define (ssl-options->ssl-tcp@ ssl-options)
  (match-define (websocket-ssl-options cert-file key-file) ssl-options)
  (define-unit-binding ssl-tcp@
    (make-ssl-tcp@ cert-file key-file #f #f #f #f #f)
    (import)
    (export tcp^))
  ssl-tcp@)

(define (spawn-websocket-listener server-addr)
  (match-define (websocket-local-server port ssl-options) server-addr)
  (define shutdown-procedure (ws-serve #:port port
				       #:tcp@ (if ssl-options
						  (ssl-options->ssl-tcp@ ssl-options)
						  tcp@)
				       (connection-handler server-addr)))
  (spawn #:name (list 'drivers/websocket:listen port)
         websocket-listener
	 (listener-state shutdown-procedure server-addr)
         (patch-seq
          (sub (advertise (observe (websocket-message ? server-addr ?)))) ;; monitor peer
          (pub (advertise (websocket-message ? server-addr ?))) ;; declare we might make connections
          (sub (inbound (websocket-connection ? server-addr ? ? ?))) ;; events from driver thd
          )))

(define (spawn-websocket-connection local-addr remote-addr)
  (match-define (websocket-remote-server url) remote-addr)
  (define id (gensym 'ws))
  (define control-ch (make-channel))
  (thread
   (lambda ()
     (log-info "Connecting to ~a ~a" url (current-inexact-milliseconds))
     (define c (with-handlers [(exn? values)] (ws-connect (string->url url))))
     (when (exn? c)
       (log-info "Connection to ~a failed: ~a" url (exn->string c)))
     (send-ground-message
      (websocket-connection id local-addr remote-addr c control-ch))
     (when (not (exn? c))
       (log-info "Connected to ~a ~a" url (current-inexact-milliseconds))
       (connection-thread-loop control-ch c id))))
  (spawn #:name (list 'drivers/websocket:connect/initial local-addr remote-addr id)
         (lambda (e buffered-messages-rev)
           (match e
             [(message (inbound (websocket-connection _ _ _ c _)))
              (quit
               (when (not (exn? c))
                 (for [(m (reverse buffered-messages-rev))] (ws-send! c m))
                 (spawn-connection local-addr remote-addr id c control-ch)))]
             [(message (websocket-message _ _ m))
              (transition (cons m buffered-messages-rev) '())]
             [_ #f]))
         '()
         (patch-seq
          (pub (websocket-message remote-addr local-addr ?))
          ;; ^ important to assert this here. If we don't, and the
          ;; connection fails, then the requesting user client actor
          ;; will hang indefinitely waiting for a sign the connection
          ;; has been established. This way, if the connection fails,
          ;; it looks like it came up briefly and went down again.
          (sub (websocket-message local-addr remote-addr ?))
          (sub (inbound (websocket-connection id local-addr remote-addr ? control-ch))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection

(struct connection-state (local-addr remote-addr c control-ch) #:transparent)

(define (shutdown-connection! state)
  (channel-put (connection-state-control-ch state) 'quit)
  (quit))

(define (websocket-connection-behaviour e state)
  (with-handlers [((lambda (exn) #t)
		   (lambda (exn)
		     (shutdown-connection! state)
		     (raise exn)))]
    (match e
      [(message (inbound (websocket-incoming-message _ (? eof-object?))))
       (shutdown-connection! state)]
      [(message (inbound (websocket-incoming-message _ bytes-or-string)))
       (transition state (message (websocket-message (connection-state-remote-addr state)
                                                     (connection-state-local-addr state)
                                                     bytes-or-string)))]
      [(message (websocket-message _ _ m))
       (ws-send! (connection-state-c state) m)
       #f]
      [(? patch/removed?)
       (shutdown-connection! state)]
      [(? patch/added?)
       (channel-put (connection-state-control-ch state) 'unblock)
       #f]
      [_ #f])))

(define (spawn-connection local-addr remote-addr id c control-ch)
  (spawn #:name (list 'drivers/websocket:connect local-addr remote-addr id)
         websocket-connection-behaviour
	 (connection-state local-addr remote-addr c control-ch)
         (patch-seq
          (let-values (((la lp ra rp) (ws-conn-peer-addresses c)))
            (assert (websocket-peer-details local-addr remote-addr la lp ra rp)))
          (sub (observe (websocket-message remote-addr local-addr ?))) ;; monitor peer
          (pub (websocket-message remote-addr local-addr ?)) ;; may send messages to peer
          (sub (websocket-message local-addr remote-addr ?)) ;; want segments from peer
          (sub (inbound (websocket-incoming-message id ?))) ;; segments from driver thd
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-websocket-driver)
