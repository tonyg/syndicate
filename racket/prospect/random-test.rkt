#lang racket/base
;; Random testing based on data/enumerate

(provide (struct-out exn:fail:reject-test)
         (struct-out property-check-result)
         (struct-out property-check-result:ok)
         (struct-out property-check-result:fail)
         (struct-out property-check-result:gave-up)
         (struct-out property-check-result:invalid-prop)
         random-test:max-tests
         random-test:max-rejected-ratio
         random-test:index-limit
         random-test:quiet?
         check-property*
         check-property
         reject-test
         ==>
         random-instance)

(require racket/match)
(require rackunit)
(require data/enumerate)
(require data/enumerate/lib)

(define-logger random-test)

(struct exn:fail:reject-test exn:fail ())

(struct property-check-result (name prop) #:transparent)
(struct property-check-result:ok property-check-result (test-count rejected-count) #:transparent)
(struct property-check-result:fail property-check-result (test-count test-case test-case-index)
  #:transparent)
(struct property-check-result:gave-up property-check-result:ok () #:transparent)
(struct property-check-result:invalid-prop property-check-result:fail () #:transparent)

(define random-test:max-tests (make-parameter 100))
(define random-test:max-rejected-ratio (make-parameter 10))
(define random-test:index-limit (make-parameter #f))
(define random-test:quiet? (make-parameter #f))

(define (summarise-check-count name n-tests n-rejected)
  (printf "\rChecking ~a; ~a tests / ~a rejected" name n-tests n-rejected)
  (flush-output))

(define (check-property* prop
                         #:max-tests [max-tests (random-test:max-tests)]
                         #:max-rejected-ratio [max-rejected-ratio (random-test:max-rejected-ratio)]
                         #:min-rejected-tests [min-rejected-tests (* max-rejected-ratio max-tests)]
                         #:index-limit [index-limit (random-test:index-limit)]
                         #:name [name (object-name prop)]
                         #:quiet? [quiet? (random-test:quiet?)]
                         . enums)
  (define args/e (apply list/e enums))
  (begin0
      (let loop ((n-tests 0)
                 (n-rejected 0))
        (define rejected-ratio (if (zero? n-tests) 0 (/ n-rejected n-tests)))
        (cond
          [(and (>= n-rejected min-rejected-tests)
                (>= rejected-ratio max-rejected-ratio))
           (unless quiet? (summarise-check-count name n-tests n-rejected))
           (property-check-result:gave-up name prop n-tests n-rejected)]
          [(>= n-tests max-tests)
           (unless quiet? (summarise-check-count name n-tests n-rejected))
           (property-check-result:ok name prop n-tests n-rejected)]
          [else
           (define args-index (if index-limit (random index-limit) (random-index args/e)))
           (define args (from-nat args/e args-index))
           (with-handlers [[exn:fail:reject-test? (lambda (_exn) (loop n-tests (+ n-rejected 1)))]]
             (unless quiet? (summarise-check-count name n-tests n-rejected))
             (match (apply prop args)
               [#t
                (unless quiet?
                  (log-random-test-debug "Passed test ~a with index ~a and args ~a"
                                         name
                                         args-index
                                         args))
                (loop (+ n-tests 1) n-rejected)]
               [#f (property-check-result:fail name prop n-tests args args-index)]
               [_ (property-check-result:invalid-prop name prop n-tests args args-index)]))]))
    (unless quiet? (newline))))

(define (signal-failure f message)
  (match-define (property-check-result:fail name prop n-tests args args-index) f)
  (with-check-info*
    (list* (check-info 'property-name name)
           (check-info 'property-prop prop)
           (check-info 'after-n-tests n-tests)
           (check-info 'test-case-index args-index)
           (for/list [(i (in-naturals)) (a (in-list args))]
             (check-info (string->symbol (format "property-arg-~a" i)) a)))
    (lambda () (fail-check message))))

(define (check-property prop
                        #:max-tests [max-tests (random-test:max-tests)]
                        #:max-rejected-ratio [max-rejected-ratio (random-test:max-rejected-ratio)]
                        #:min-rejected-tests [min-rejected-tests (* max-rejected-ratio max-tests)]
                        #:index-limit [index-limit (random-test:index-limit)]
                        #:name [name (object-name prop)]
                        #:quiet? [quiet? (random-test:quiet?)]
                        . enums)
  ((current-check-around)
   (lambda ()
     (define result (apply check-property* prop enums
                           #:max-tests max-tests
                           #:max-rejected-ratio max-rejected-ratio
                           #:min-rejected-tests min-rejected-tests
                           #:index-limit index-limit
                           #:name name
                           #:quiet? quiet?))
     (match result
       [(? property-check-result:invalid-prop? f)
        (signal-failure f (format "Invalid property ~a (expected #t or #f)" name))]
       [(? property-check-result:fail? f)
        (signal-failure f (format "Failed check of property ~a" name))]
       [(property-check-result:gave-up name prop n-tests n-rejected)
        (log-random-test-warning
         "Gave up checking property ~a after ~a passed tests / ~a rejected tests"
         name
         n-tests
         n-rejected)]
       [(property-check-result:ok name prop n-tests n-rejected)
        (log-random-test-info
         "OK property ~a after ~a passed tests / ~a rejected tests"
         name
         n-tests
         n-rejected)]))))

(define (reject-test [message "Rejecting test case because of precondition failure"])
  (raise (exn:fail:reject-test message (current-continuation-marks))))

(define-syntax-rule (==> precondition expr)
  (if (not precondition) (reject-test) expr))

(define (random-instance e)
  (from-nat e (random-index e)))
