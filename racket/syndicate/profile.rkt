#lang racket/base

(provide profile-dataspace!
         profile-lambda)

(require syndicate/lang)
(require profile)

(define profile-lambda
  (make-keyword-procedure
   (lambda (profile-ks profile-vs procedure)
     (make-keyword-procedure
      (lambda (invocation-ks invocation-vs . invocation-positionals)
        (keyword-apply profile-thunk
                       profile-ks
                       profile-vs
                       (list (lambda ()
                               (keyword-apply procedure
                                              invocation-ks
                                              invocation-vs
                                              invocation-positionals)))))))))

(define profile-dataspace!
  (make-keyword-procedure
   (lambda (ks vs . positionals)
     (current-ground-dataspace
      (keyword-apply profile-lambda ks vs (current-ground-dataspace) positionals)))))
