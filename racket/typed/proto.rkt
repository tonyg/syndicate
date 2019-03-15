#lang racket

(require (only-in racket/hash hash-union))

(module+ test
  (require rackunit))

;; a T is one of
;;   - (Role FacetName (Listof EP)), also abbreviated as just Role
;;   - (Spawn τ)
;;   - (Stop FacetName)
(struct Role (nm eps) #:transparent)
(struct Spawn (ty) #:transparent)
(struct Stop (nm) #:transparent)

;; a FacetName is a symbol

;; a EP is one of
;;   - (Reacts D (Listof T)), describing an event handler
;;   - (Shares τ), describing an assertion
(struct Reacts (evt body) #:transparent)
(struct Shares (ty) #:transparent)

;; a D is one of
;;   - (Know τ), reaction to assertion
;;   - (¬Know τ), reaction to retraction
(struct Know (ty) #:transparent)
(struct ¬Know (ty) #:transparent)

;; a τ is one of
;;   - (U τ ...)
;;   - (Struct StructName (Listof τ ...))
;;   - (Observe τ)
;;   - ⋆
;;   - Int
;;   - String
(struct U (tys) #:transparent)
(struct Struct (nm tys) #:transparent)
(struct Observe (ty) #:transparent)
(struct Mk⋆ () #:transparent)
;; TODO this might be a problem when used as a match pattern
(define ⋆ (Mk⋆))
(struct MkInt () #:transparent)
(define Int (MkInt))
(struct MkString () #:transparent)
(define String (MkString))

;; a StructName is a Symbol

;; --------------------------------------------------------------------------
;; Examples

(define manager
  (Role 'account-manager
        (list (Shares (Struct 'account (list Int)))
              (Reacts (Know (Struct 'deposit '())) '()))))
(define client
  (Role 'client
        (list (Reacts (Know (Struct 'account (list Int))) '()))))

(define seller
  (Role 'seller
        (list
         (Reacts (Know (Observe (Struct 'BookQuoteT (list String ⋆))))
                 (list
                  (Role 'fulfill
                        (list (Shares (Struct 'BookQuoteT (list String Int))))))))))


;; a State is a (state StateName (Hashof D StateName))
;; a StateName is a (Setof FacetName)
;; let's assume that all FacetNames are unique
;; ok, this is also ignoring Spawn actions for now, would show up in the transitions hash
(struct state (name transitions) #:transparent)

;; -----------------------------------------------------------------------------
;; Compiling Roles to state machines

;; Role -> (Hashof StateName State)
;; in each state, the transitions will include the reactions of the parent
;; facet(s)
(define (compile role)
  (define roles# (describe-roles role))
  (let loop ([work-list (list (set (Role-nm role)))]
             [states (hash)])
    (match work-list
      [(cons current more)
       (define all-txns
         (for/list ([nm (in-set current)])
           (hash-ref roles# nm)))
       (define agg-txn
         (for/fold ([agg (hash)])
                   ([txns (in-list all-txns)])
           (hash-union agg txns
                       #:combine append)))
       (define transitions
         (for/hash ([(D effs) (in-hash agg-txn)])
           (values D (apply-effects effs current))))
       (define new-work
         (for/list ([st (in-hash-values transitions)]
                    #:unless (hash-has-key? states st))
           st))
       (loop (append more new-work)
             (hash-set states current (state current transitions)))]
      ['()
       states])))

(module+ test
  (test-case
      "compile seller"
    (define seller# (compile seller))
    (check-true (hash-has-key? seller# (set 'seller)))
    (check-true (hash-has-key? seller# (set 'seller 'fulfill)))
    (check-equal? (hash-keys seller#)
                  (list (set 'seller 'fulfill)
                        (set 'seller)))
    (define st0 (hash-ref seller# (set 'seller)))
    (define transitions (state-transitions st0))
    (define quote-request
      (Observe (Struct 'BookQuoteT (list String ⋆))))
    (check-true (hash-has-key? transitions (Know quote-request)))
    (check-equal? (hash-ref transitions (Know quote-request))
                  (set 'seller 'fulfill))))

;; a RoleEffect is one of
;;   - (start RoleName)
;;   - (stop RoleName)
;; TODO - leaving out Spawn here
(struct start (nm) #:transparent)
(struct stop (nm) #:transparent)

;; a TransitionDesc is a (Hashof D (Listof RoleEffect)), describing when
;; transitions occur (+/- of an assertion) and how they alter the facet tree.

;; (Listof RoleEffect) StateName -> StateName
;; determine the state resulting from some effects
(define (apply-effects effs st)
  (for/fold ([st st])
            ([eff (in-list effs)])
    (match eff
      [(start nm)
       (set-add st nm)]
      [(stop nm)
       (set-remove st nm)])))

;; Role -> (Hashof FacetName TransitionDesc)
;; Extract a description of all roles mentioned in a Role
(define (describe-roles role)
  (let loop ([roles (list role)]
             [desc (hash)])
    (match roles
      [(cons role roles)
       (match-define (Role nm eps) role)
       (define txn (describe-role role))
       (define reacts (filter Reacts? eps))
       (define more-roles
         (for*/list ([react (in-list reacts)]
                     [body (in-value (Reacts-body react))]
                     [act (in-list body)]
                     #:when (Role? act))
           act))
       (loop (append roles more-roles)
             (hash-set desc nm txn))]
      ['()
       desc])))

;; Role -> TransitionDesc
;; determine how the event handlers in a role alter the facet tree
(define (describe-role role)
  (match role
    [(Role nm eps)
     (for/fold ([txns (hash)])
               ([ep (in-list eps)]
                #:when (Reacts? ep))
       (match-define (Reacts evt acts) ep)
       (define effects
         (for/list ([act (in-list acts)]
                    #:when (or (Role? act)
                               ;; TODO - need to account for Spawn here at some point
                               (Stop? act)))
           (match act
             [(Role nm2 _) (start nm2)]
             [(Stop nm2) (stop nm2)])))
       (cond
         [(empty? effects)
          txns]
         [else
          (hash-update txns evt ((curry append) effects) '())]))]))

(module+ test
  (test-case
      "describe simple role"
    (define desc (describe-roles manager))
    (check-true (hash-has-key? desc 'account-manager))
    (check-equal? (hash-ref desc 'account-manager)
                  (hash)))
  (test-case
      "describe nested role"
    (define desc (describe-roles seller))
    (check-true (hash-has-key? desc 'seller))
    (check-true (hash-has-key? desc 'fulfill))
    (check-equal? (hash-ref desc 'fulfill)
                  (hash))
    (define seller-txns (hash-ref desc 'seller))
    (define quote-request
      (Observe (Struct 'BookQuoteT (list String ⋆))))
    (check-true (hash-has-key? seller-txns (Know quote-request)))
    (check-equal? (hash-ref seller-txns (Know quote-request))
                  (list (start 'fulfill)))))
