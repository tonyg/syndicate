#lang syndicate/core

(require racket/exn)
(require (prefix-in tcp: racket/tcp))
(require (only-in racket/port read-bytes-avail!-evt))
(require syndicate/demand-matcher)
(require syndicate/protocol/advertise)

(require racket/unit)
(require net/tcp-sig)
(require net/tcp-unit)

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 spawn-tcp-driver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct tcp-address (host port) #:prefab)
(struct tcp-handle (id) #:prefab)
(struct tcp-listener (port) #:prefab)

(struct tcp-channel (source destination subpacket) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground-level communication messages

(struct tcp-accepted (remote-addr local-addr cin cout) #:prefab)
;;      tcp-channel does double-duty as a ground-level message as well

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver

(define (spawn-tcp-driver)
  (list (spawn-demand-matcher (advertise (observe (tcp-channel ? (?! (tcp-listener ?)) ?)))
                              (advertise (advertise (tcp-channel ? (?! (tcp-listener ?)) ?)))
			      spawn-tcp-listener
                              #:name 'drivers/tcp:dm:listener)
	(spawn-demand-matcher (advertise (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?))
                              (observe (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?))
			      spawn-tcp-connection
                              (lambda (local-addr remote-addr)
                                (log-debug "Outbound TCP connection closed: ~v -> ~v"
                                           local-addr
                                           remote-addr))
                              #:name 'drivers/tcp:dm:connect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener

(struct listener-state (control-ch server-addr) #:transparent)

(define (tcp-listener-thread control-ch listener server-addr)
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt (tcp:tcp-accept-evt listener)
			  (lambda (cin+cout)
			    (match-define (list cin cout) cin+cout)
			    (define-values (local-hostname local-port remote-hostname remote-port)
			      (tcp:tcp-addresses cin #t))
			    (send-ground-message
			     (tcp-accepted (tcp-address remote-hostname remote-port)
					   server-addr
					   cin
					   cout))
			    (loop blocked?))))))
  (tcp:tcp-close listener))

(define (tcp-listener-behavior e state)
  (match e
    [(? patch? p)
     (define ch (listener-state-control-ch state))
     (cond [(patch/removed? p) (channel-put ch 'quit) (quit)]
           [(patch/added? p) (channel-put ch 'unblock) #f]
           [else #f])]
    [(message (inbound (tcp-accepted remote-addr _ cin cout)))
     (transition state (spawn-connection (listener-state-server-addr state)
					 remote-addr
					 cin
					 cout))]
    [_ #f]))

(define (spawn-tcp-listener server-addr)
  (match-define (tcp-listener port) server-addr)
  (define listener (tcp:tcp-listen port 128 #t))
  (define control-ch (make-channel))
  (thread (lambda () (tcp-listener-thread control-ch listener server-addr)))
  (actor #:name (list 'drivers/tcp:listen port)
         tcp-listener-behavior
	 (listener-state control-ch server-addr)
         (patch-seq
          (sub (advertise (observe (tcp-channel ? server-addr ?)))) ;; monitor peer
          (pub (advertise (tcp-channel ? server-addr ?))) ;; declare we might make connections
          (sub (inbound (tcp-accepted ? server-addr ? ?))) ;; events from driver thread
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outbound Connection

(define (spawn-tcp-connection local-addr remote-addr)
  (match-define (tcp-address remote-hostname remote-port) remote-addr)
  (define-values (cin cout)
    (with-handlers ([exn:fail:network? (lambda (e)
					 ;; TODO: it'd be nice to
					 ;; somehow communicate the
					 ;; actual error to the local
					 ;; peer.
					 (log-error "~a" (exn->string e))
					 (define o (open-output-string))
					 (close-output-port o)
					 (values (open-input-string "")
						 o))])
      (tcp:tcp-connect remote-hostname remote-port)))
  (spawn-connection local-addr remote-addr cin cout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection

(struct connection-state (control-ch cout) #:transparent)

(define (read-bytes-avail-evt len input-port)
  (guard-evt
   (lambda ()
     (let ([bstr (make-bytes len)])
       (handle-evt
        (read-bytes-avail!-evt bstr input-port)
        (lambda (v)
          (if (number? v)
              (if (= v len) bstr (subbytes bstr 0 v))
              v)))))))

(define (tcp-connection-thread remote-addr local-addr control-ch cin)
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt (read-bytes-avail-evt 32768 cin)
			  (lambda (eof-or-bs)
			    (send-ground-message (tcp-channel remote-addr local-addr eof-or-bs))
			    (loop (or blocked? (eof-object? eof-or-bs))))))))
  (close-input-port cin))

(define (shutdown-connection! state)
  (match-define (connection-state control-ch cout) state)
  (when control-ch (channel-put control-ch 'quit))
  (when cout (close-output-port cout)))

(define (tcp-connection e state)
  (with-handlers [((lambda (exn) #t)
		   (lambda (exn)
		     (shutdown-connection! state)
		     (raise exn)))]
    (match e
      [(message (inbound (tcp-channel remote-addr local-addr (? eof-object?))))
       (shutdown-connection! state)
       (quit)]
      [(message (inbound (tcp-channel remote-addr local-addr (? bytes? bs))))
       (transition state (message (tcp-channel remote-addr local-addr bs)))]
      [(message (tcp-channel _ _ bs))
       (write-bytes bs (connection-state-cout state))
       (flush-output (connection-state-cout state))
       #f]
      [(? patch? p)
       (define ch (connection-state-control-ch state))
       (cond [(patch/removed? p) (shutdown-connection! state) (quit)]
             [(patch/added? p) (channel-put ch 'unblock) #f]
             [else #f])]
      [#f #f])))

(define (spawn-connection local-addr remote-addr cin cout)
  (define control-ch (make-channel))
  (thread (lambda () (tcp-connection-thread remote-addr local-addr control-ch cin)))
  (actor #:name (list 'drivers/tcp:connect local-addr remote-addr)
         tcp-connection
	 (connection-state control-ch cout)
         (patch-seq
          (sub (observe (tcp-channel remote-addr local-addr ?))) ;; monitor peer
          (pub (tcp-channel remote-addr local-addr ?)) ;; may send segments to peer
          (sub (tcp-channel local-addr remote-addr ?)) ;; want segments from peer
          (sub (inbound (tcp-channel remote-addr local-addr ?))) ;; segments from driver thread
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-tcp-driver)
