#lang racket/base

(require racket/match)
(require net/rfc6455)
(require (only-in net/rfc6455/conn-api ws-conn-base-ip))
(require "../main.rkt")
(require "../demand-matcher.rkt")

(require racket/unit)
(require net/tcp-sig)
(require net/tcp-unit)
(require net/ssl-tcp-unit)
(require net/url)

(provide (struct-out websocket-remote-client)
	 (struct-out websocket-local-server)
	 (struct-out websocket-local-client)
	 (struct-out websocket-remote-server)
	 (struct-out websocket-ssl-options)
	 (struct-out websocket-message)
	 spawn-websocket-driver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct websocket-remote-client (id) #:prefab)
(struct websocket-local-server (port ssl-options) #:prefab)
(struct websocket-local-client (id) #:prefab)
(struct websocket-remote-server (url) #:prefab)
(struct websocket-ssl-options (cert-file key-file) #:prefab)
(struct websocket-message (from to body) #:prefab)

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
                              spawn-websocket-listener)
        (spawn-demand-matcher (advertise outbound-conn-message-pat)
                              (observe outbound-conn-message-pat)
                              spawn-websocket-connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener

(struct listener-state (shutdown-procedure server-addr) #:transparent)

(define (websocket-listener e state)
  (match e
    [(? patch/removed? p)
     ((listener-state-shutdown-procedure state))
     (quit)]
    [(message (at-meta (websocket-connection id local-addr remote-addr c control-ch)))
     (transition state (spawn-connection local-addr remote-addr id c control-ch))]
    [_ #f]))

(define ((connection-handler server-addr) c dummy-state)
  (define control-ch (make-channel))
  (define id (gensym 'ws))
  (send-ground-message
   (websocket-connection id server-addr (websocket-remote-client id) c control-ch))
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
			      (with-handlers ([exn:fail:network? (lambda (e) eof)])
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
  (spawn websocket-listener
	 (listener-state shutdown-procedure server-addr)
         (sub (advertise (observe (websocket-message ? server-addr ?)))) ;; monitor peer
         (pub (advertise (websocket-message ? server-addr ?))) ;; declare we might make connections
         (sub (websocket-connection ? server-addr ? ? ?) #:meta-level 1) ;; events from driver thd
         ))

(define (spawn-websocket-connection local-addr remote-addr)
  (match-define (websocket-remote-server url) remote-addr)
  (define id (gensym 'ws))
  (define control-ch (make-channel))
  (thread
   (lambda ()
     (log-info "Connecting to ~a ~a" url (current-inexact-milliseconds))
     (define c (with-handlers [(exn? values)] (ws-connect (string->url url))))
     (log-info "Connected to ~a ~a" url (current-inexact-milliseconds))
     (send-ground-message
      (websocket-connection id local-addr remote-addr c control-ch))
     (when (not (exn? c))
       (connection-thread-loop control-ch c id))))
  (spawn (lambda (e buffered-messages-rev)
           (match e
             [(message (at-meta (websocket-connection _ _ _ c _)))
              (when (not (exn? c))
                (for [(m (reverse buffered-messages-rev))] (ws-send! c m))
                (spawn-connection local-addr remote-addr id c control-ch))
              (quit)]
             [(message (websocket-message _ _ m))
              (transition (cons m buffered-messages-rev) '())]
             [_ #f]))
         '()
         (sub (websocket-connection id local-addr remote-addr ? control-ch) #:meta-level 1)
         (sub (websocket-message local-addr remote-addr ?))))

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
      [(message (at-meta (websocket-incoming-message _ (? eof-object?))))
       (shutdown-connection! state)]
      [(message (at-meta (websocket-incoming-message _ (? bytes? bs))))
       (transition state (message (websocket-message (connection-state-remote-addr state)
                                                     (connection-state-local-addr state)
                                                     bs)))]
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
  (spawn websocket-connection-behaviour
	 (connection-state local-addr remote-addr c control-ch)
         (sub (observe (websocket-message remote-addr local-addr ?))) ;; monitor peer
	 (pub (websocket-message remote-addr local-addr ?)) ;; may send messages to peer
         (sub (websocket-message local-addr remote-addr ?)) ;; want segments from peer
         (sub (websocket-incoming-message id ?) #:meta-level 1) ;; segments from driver thd
         ))
