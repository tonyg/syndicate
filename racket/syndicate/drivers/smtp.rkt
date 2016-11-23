#lang syndicate/actor

(provide (struct-out/defaults [make-smtp-account-config smtp-account-config])
         (rename-out [smtp-account-config <smtp-account-config>])
         (struct-out smtp-account)
         (struct-out smtp-delivery)
         (struct-out smtp-delivery-complete)

         spawn-smtp-driver
         smtp-deliver!)

(define-logger syndicate/drivers/smtp)

(require racket/exn)
(require racket/tcp)
(require (only-in racket/list flatten))
(require net/head)
(require net/smtp)
(require openssl)
(require struct-defaults)

;; An SSLMode is one of
;; - #f: Use unencrypted SMTP, by default at port 587
;; - 'ssl: Use TLS-tunneled SMTP, by default at port 465 (!)
;; - 'starttls: Use STARTTLS SMTP upgrade to TLS encryption, by default at port 587

;; (smtp-account-config Symbol String Number (Option String) (Option String) SSLMode)
(struct smtp-account-config (id host port user password ssl-mode) #:prefab) ;; ASSERTION

;; (smtp-account Any)
(struct smtp-account (id) #:prefab) ;; ASSERTION

(struct smtp-delivery (account-id ;; Any
                       delivery-id ;; Any
                       from ;; String -- *envelope* FROM
                       to ;; (Listof String) -- *envelope* RCPT
                       header ;; (Listof (Cons Symbol String))
                       lines ;; (Listof (U String Bytes))
                       ) #:prefab) ;; MESSAGE

;; (smtp-delivery-complete Any Boolean)
(struct smtp-delivery-complete (delivery-id ok?) #:prefab) ;; MESSAGE

;; On SMTP ports.
;;
;; 25: MTA-to-MTA; message transfer
;; 465: Legacy SSL SMTP, prefer not to use
;; 587: MUA-to-MTA; message submission; STARTTLS for upgrade to TLS

(begin-for-declarations
  (define-struct-defaults make-smtp-account-config smtp-account-config
    (#:ssl-mode [smtp-account-config-ssl-mode 'starttls]
     #:port [smtp-account-config-port (case smtp-account-config-ssl-mode
                                        [(#f starttls) 587]
                                        [(ssl) 465]
                                        [else (error 'smtp-account-config
                                                     "Invalid smtp-account-config-ssl-mode ~v"
                                                     smtp-account-config-ssl-mode)])]
     #:user [smtp-account-config-user #f]
     #:password [smtp-account-config-password #f])))

(define (spawn-smtp-driver)
  (actor #:name 'smtp-account-driver
         (during/actor (smtp-account-config $id $host $port $user $password $ssl-mode)
           #:name (list 'smtp-account id)
           (on-start
            (log-syndicate/drivers/smtp-info "~v starting: ~s ~s ~s" id host user ssl-mode))
           (on-stop
            (log-syndicate/drivers/smtp-info "~v stopping: ~s ~s ~s" id host user ssl-mode))
           (assert (smtp-account id))
           (on (message (smtp-delivery id $delivery-id $from $to $header $lines))
               (with-handlers [(exn:fail?
                                (lambda (e)
                                  (log-syndicate/drivers/smtp-error "smtp-delivery ~a ~a: ~a"
                                                                    id
                                                                    delivery-id
                                                                    (exn->string e))
                                  (send-ground-message
                                   (smtp-delivery-complete delivery-id #f))))]
                 (parameterize ((smtp-sending-end-of-message
                                 (lambda ()
                                   (send-ground-message
                                    (smtp-delivery-complete delivery-id #t)))))
                   (log-syndicate/drivers/smtp-info "account ~a delivery ~a: ~s -> ~s"
                                                    id
                                                    delivery-id
                                                    from
                                                    to)
                   (smtp-send-message host
                                      from
                                      to
                                      (construct-header header)
                                      lines
                                      #:port-no port
                                      #:auth-user user
                                      #:auth-passwd password
                                      #:tcp-connect (case ssl-mode
                                                      [(ssl) ssl-connect]
                                                      [else tcp-connect])
                                      #:tls-encode (case ssl-mode
                                                     [(starttls) ports->ssl-ports]
                                                     [else #f]))))))
         (during/actor (smtp-account-config _ _ _ _ _ _)
           ;; By *conditionally* paying attention to inbound messages
           ;; from ground, we ensure that we don't unnecessarily hold
           ;; up ground termination.
           (on (message (inbound (smtp-delivery-complete $delivery-id $ok?)))
               (send! (smtp-delivery-complete delivery-id ok?))))))

(define (construct-header hs)
  (for/fold [(acc empty-header)] [(h (reverse hs))]
    (match-define (cons key val) h)
    (insert-field (symbol->string key) val acc)))

(define (smtp-deliver! account-id from to header lines)
  (define delivery-id (gensym 'smtp-delivery))
  (react/suspend (k)
                 (on (asserted (smtp-account account-id))
                     (send! (smtp-delivery account-id delivery-id from to header lines)))
                 (stop-when (message (smtp-delivery-complete delivery-id $status))
                            (k status))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-smtp-driver)
