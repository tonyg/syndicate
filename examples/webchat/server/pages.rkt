#lang syndicate/actor

(require racket/dict)
(require racket/port)
(require racket/set)
(require racket/string)
(require markdown)
(require net/url)
(require net/uri-codec)

(require/activate syndicate/reload)
(require/activate syndicate/drivers/config)
(require/activate syndicate/drivers/smtp)
(require/activate syndicate/drivers/timestate)
(require/activate syndicate/drivers/web)

(require "protocol.rkt")
(require "session-cookie.rkt")

(define (page #:head [extra-head '()]
              #:body-id [body-id #f]
              #:nav-heading [nav-heading "Syndicate Webchat"]
              title . body-elements)
  `(html ((lang "en"))
         (head (meta ((charset "utf-8")))
               (meta ((http-equiv "X-UA-Compatible") (content "IE=edge")))
               (meta ((name "viewport") (content "width=device-width, initial-scale=1.0, shrink-to-fit=no")))
               (title ,title)
               (link ((rel "stylesheet")
                      (href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.5/css/bootstrap.min.css")
                      (integrity "sha384-AysaV+vQoT3kOAXZkl02PThvDr8HYKPZhNT5h/CXfBThSRXQ6jW5DO2ekP5ViFdi")
                      (crossorigin "anonymous")))
               (script ((src "https://code.jquery.com/jquery-3.1.1.min.js")
                        (integrity "sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8=")
                        (crossorigin "anonymous")))
               (script ((src "https://cdnjs.cloudflare.com/ajax/libs/tether/1.3.8/js/tether.min.js")
                        (integrity "sha256-/5pHDZh2fv1eZImyfiThtB5Ag4LqDjyittT7fLjdT/8=")
                        (crossorigin "anonymous")))
               (script ((src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.5/js/bootstrap.min.js")
                        (integrity "sha384-BLiI7JTZm+JWlgKa0M0kGRpJbF2J8q+qreVrKBC47e3K6BW78kGLrCkeRX6I9RoK")
                        (crossorigin "anonymous")))
               (script ((src "https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js")
                        (integrity "sha256-iaqfO5ue0VbSGcEiQn+OeXxnxAMK2+QgHXIDA5bWtGI=")
                        (crossorigin "anonymous")))
               (script ((src "https://cdnjs.cloudflare.com/ajax/libs/blueimp-md5/2.6.0/js/md5.min.js")
                        (integrity "sha256-I0CACboBQ1ky299/4LVi2tzEhCOfx1e7LbCcFhn7M8Y=")
                        (crossorigin "anonymous")))
               ;; (script ((src "/syndicatecompiler.min.js")))
               (script ((src "/syndicate.min.js")))
               (script ((src "/webchat.js")))
               (link ((rel "stylesheet") (href "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css")))
               (link ((rel "stylesheet") (href "/style.css")))
               ,@extra-head)
         (body (,@(if body-id
                      `((id ,body-id))
                      `()))
               (div ((class "container"))
                    (div ((class "header clearfix"))
                         (nav (ul ((id "nav-ul") (class "nav nav-pills float-xs-right"))
                                  ;; (li ((class "nav-item")) (a ((class "nav-link active") (href "#")) "Home " (span ((class "sr-only")) "(current)")))
                                  ;; (li ((class "nav-item")) (a ((class "nav-link") (href "#")) "About"))
                                  ;; (li ((class "nav-item")) (a ((class "nav-link") (href "#")) "Contact"))
                                  ))
                         (h3 ((id "nav-heading") (class "text-muted")) ,nav-heading))

                    (div ((id "main-div")))
                    ;; (div ((class "row marketing"))
                    ;;      (div ((class "col-lg-6"))
                    ;;           (h4 "Subheading")
                    ;;           (p "Donec id elit non mi porta gravida at eget metus. Maecenas faucibus mollis interdum.")
                    ;;           (h4 "Subheading")
                    ;;           (p "Morbi leo risus, porta ac consectetur ac, vestibulum at eros. Cras mattis consectetur purus sit amet fermentum.")
                    ;;           (h4 "Subheading")
                    ;;           (p "Maecenas sed diam eget risus varius blandit sit amet non magna."))
                    ;;      (div ((class "col-lg-6"))
                    ;;           (h4 "Subheading")
                    ;;           (p "Morbi leo risus, porta ac consectetur ac, vestibulum at eros. Cras mattis consectetur purus sit amet fermentum.")
                    ;;           (h4 "Subheading")
                    ;;           (p "Maecenas sed diam eget risus varius blandit sit amet non magna.")
                    ;;           (h4 "Subheading")
                    ;;           (p "Donec id elit non mi porta gravida at eget metus. Maecenas faucibus mollis interdum.")))

                    ,@body-elements

                    (footer ((class "footer"))
                            (p copy " 2010" ndash "2016 Tony Garnock-Jones"))))))

(define (jumbotron heading . contents)
  `(div ((class "jumbotron"))
        (h1 ((class "display-3")) ,heading)
        ,@contents))

(define (logout-this-session! id)
  (web-redirect! id "/" #:headers (list (format-cookie clear-session-cookie))))

(define (web-respond/pretty-xexpr! id
                                   #:header [header (web-response-header)]
                                   body-xexpr)
  (web-respond/bytes! id
                      #:header header
                      (bytes-append #"<!DOCTYPE html>"
                                    (with-output-to-bytes
                                      (lambda ()
                                        ;; This is a very nice compromise pretty-printer
                                        ;; for xexprs from Greg's Markdown package.
                                        (display-xexpr body-xexpr))))))

(actor #:name 'index-page
       (stop-when-reloaded)
       (on (web-request-get (id req) _ ("" ()))
           (index-page id)))

(define (index-page id)
  (with-session id
    [(email sid)
     (serve-single-page-app id sid email)]
    [else
     (web-respond/pretty-xexpr!
      id
      #:header (web-response-header #:headers (list (format-cookie clear-session-cookie)))
      (page "Syndicate Webchat"
            (jumbotron "Log In"
                       `(p ((class "lead"))
                           "Enter your email address. You will be emailed a login token.")

                       `(form ((action "/login") (method "post") (class "form-inline"))
                              (div ((class "form-group"))
                                   (label ((for "email")) "Email:")
                                   " "
                                   (input ((type "email")
                                           (name "email")
                                           (id "email")
                                           (value "tonyg@ccs.neu.edu") ;; TODO
                                           (placeholder "your-email@example.com"))))
                              " "
                              (button ((type "submit")
                                       (class "btn btn-success")
                                       (role "button"))
                                      "Log In")))))]))

(define (serve-single-page-app id sid email)
  (web-respond/pretty-xexpr!
   id
   (page (format "Webchat: ~a" email)
         #:body-id "webchat-main"
         #:nav-heading email
         #:head (list `(meta ((itemprop "webchat-session-email") (content ,email)))
                      `(meta ((itemprop "webchat-session-id") (content ,sid)))))))

;; (define (sessions-page id)
;;   (with-session id
;;     [(email sid)
;;      (define sids (sort (set->list (immediate-query (query-set (session email $s) s))) string<?))
;;      (web-respond/pretty-xexpr!
;;       id
;;       (page "Session Management"
;;             `(div (h1 "Session Management")
;;                   (ol ,@(for/list [(s sids)]
;;                           `(li (a ((href ,(format "/logout/~a" s)))
;;                                   ,s))))
;;                   (p (a ((href "/logout-all"))
;;                         "Logout all sessions"))
;;                   (p (a ((href "/delete-account"))
;;                         "Delete account")))))]))

;; (define (logout-all-page id)
;;   (with-session id
;;     [(email _sid)
;;      (for [(sid (immediate-query (query-set (session email $s) s)))]
;;        (send! (end-session sid)))
;;      (logout-this-session! id)]
;;     [else (logout-this-session! id)]))

(actor #:name 'logout-page
       (stop-when-reloaded)
       (on (web-request-get (id req) _ ("logout" ()))
           (logout-page id)))

(define (logout-page id)
  (with-session id
    [(email sid)
     (send! (end-session sid))
     (logout-this-session! id)]
    [else (logout-this-session! id)]))

(actor #:name 'login-page
       (stop-when-reloaded)
       (define/query-value insecure #f (config _ (list 'insecure)) #t)
       (define/query-value baseurl #f (server-baseurl $b) b)

       (on (web-request-incoming (id req) _ 'post ("login" ()) $body)
           (define params (make-immutable-hash (form-urlencoded->alist (bytes->string/utf-8 body))))
           (define email (string-trim (dict-ref params 'email "")))
           (if (string=? email "")
               (web-redirect! id "/")
               (let* ((sid (fresh-session-id))
                      (validation-url (url->string
                                       (combine-url/relative (string->url (baseurl))
                                                             (format "/login/~a" sid)))))
                 (spawn-login-link email sid)
                 (login-link-emailed-page id (and (insecure) validation-url))
                 (when (not (insecure))
                   (smtp-deliver! 'smtp-service "webchat@syndicate-lang.org" (list email)
                                  (list (cons 'subject "Login link for Syndicate WebChat")
                                        (cons 'to email)
                                        (cons 'from "webchat@syndicate-lang.org"))
                                  (list (format "Hello ~a," email)
                                        (format "")
                                        (format "Here is your login link for Syndicate WebChat:")
                                        (format "")
                                        (format "    ~a" validation-url))))))))

(define (spawn-login-link email sid)
  (actor #:name (list 'login-link email sid)
         (on-start (log-info "Login link ~s for ~s activated." sid email))
         (on-stop (log-info "Login link ~s for ~s deactivated." sid email))
         (assert (login-link email sid))
         (stop-when (asserted (session _ sid))) ;; happy path
         (stop-when (message (end-session sid)))
         (stop-when (message (delete-account email)))
         (stop-when-timeout (* 10 1000)))) ;; 10 seconds

(define (login-link-emailed-page id maybe-insecure-validation-url)
  (web-respond/pretty-xexpr!
   id
   (page "Syndicate Webchat"
         (jumbotron "Login Link Emailed"
                    (if maybe-insecure-validation-url
                        `(p ((class "insecure-mode lead"))
                            "INSECURE MODE: Click "
                            (a ((href ,maybe-insecure-validation-url)) "here")
                            " to log in")
                        `(p ((class "lead"))
                            "A login link should appear "
                            "in your inbox shortly."))))))

(actor #:name 'login-link-page
       (stop-when-reloaded)
       ;; Can't handle the request within each login-link process, since we have to take
       ;; special action if there is no such login link, and we are not allowed to race,
       ;; meaning that this has to be a Single Point Of Control for making decisions based
       ;; on the login-link relation.
       (on (web-request-get (id req) _ ("login" (,$sid ())))
           (match (immediate-query (query-value #f (login-link $email sid) email))
             [#f (login-link-expired-page id)]
             [email
              (spawn-session-monitor email sid)
              (web-redirect! id "/" #:headers (list (format-cookie (session-id->cookie sid))))])))

(define (login-link-expired-page id)
  (web-respond/pretty-xexpr!
   id
   (page "Login Link Expired or Invalid"
         (jumbotron "Login Link Expired or Invalid"
                    `(p ((class "lead"))
                        "Please " (a ((href "/")) "return to the main page") ".")))))

(define (spawn-session-monitor email sid)
  (actor #:name (list 'session-monitor email sid)
         (on-start (log-info "Session ~s for ~s started." sid email))
         (on-stop (log-info "Session ~s for ~s stopped." sid email))
         (assert (session email sid))
         (stop-when (message (end-session sid)))
         (stop-when (message (delete-account email)))
         (stop-when-timeout (* 7 86400 1000)))) ;; 1 week
