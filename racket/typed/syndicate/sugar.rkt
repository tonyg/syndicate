#lang turnstile

(provide Observe★
         RoleNTimes
         (for-syntax RoleNTimes*))

(require "core-types.rkt")
(require turnstile/typedefs)

(define-syntax (Observe★ stx)
  (define star (type-eval #'★/t))
  (syntax-parse stx
    [(_ TyCons:id)
     #:do [(define arity? (get-type-arity #'TyCons))]
     #:when arity?
     (mk-Observe- (list (reassemble-type #'TyCons (make-list (arity-min arity?) star))))]
    [(_ (~Any/new TyCons τ ...))
     #:when (reassemblable? #'TyCons)
     (mk-Observe- (list (reassemble-type #'TyCons (stx-map (lambda (_) star) #'(τ ...)))))]
    [_
     (raise-syntax-error #f "Not a type that can automatically be subscribed to" stx)]))

(begin-for-syntax
  ;; Arity -> Nat
  (define (arity-min a)
    (match a
      [(arity-eq n) n]
      [(arity-ge n) n])))

(define-for-syntax (RoleNTimes* n Step behav)
  (let loop ([i 1])
  (define nm (format-id behav "step~a" i))
  (quasisyntax/loc behav
    (Role (#,nm)
      #,@(if (= i 1)
             (list #'(Shares Unit))
             (list))
      (Reacts #,(if (= i 1)
                    #'(Asserted Unit)
                    #`(Message #,Step))
              (Sends #,Step))
      (Reacts (Message #,Step)
              (Effs #,behav
                    (Stop #,nm
                          #,@(if (< i n)
                                 (list #`(Sends #,Step) (loop (add1 i)))
                                 (list)))))))))

(define-syntax-parser RoleNTimes
  [(_ ~! n:nat Step:type behav:type)
   (RoleNTimes* (syntax-e #'n) #'Step.norm #'behav.norm)])
