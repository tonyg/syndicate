#lang typed/syndicate/roles

#;(require "core-types.rkt")
#;(require "core-expressions.rkt")
#;(require "prim.rkt")
#;(require "for-loops.rkt")
#;(require "list.rkt")
#;(require "roles.rkt")
#;(require "maybe.rkt")

#;(require macro-debugger/stepper)

;; (define-type-alias ID Symbol)
;; (require-struct task #:as Task #:from "examples/roles/flink-support.rkt")
;; (require-struct map-work #:as MapWork #:from "examples/roles/flink-support.rkt")
;; (require-struct reduce-work #:as ReduceWork #:from "examples/roles/flink-support.rkt")
;; (define-type-alias TaskID (Tuple Int ID))
;; (define-type-alias WordCount (Hash String Int))
;; (define-type-alias TaskResult WordCount)
;; (define-type-alias Reduce
;;   (ReduceWork (Either Int TaskResult)
;;               (Either Int TaskResult)))
;; (define-type-alias Work
;;   (U Reduce (MapWork String)))
;; (define-type-alias ConcreteWork
;;   (U (ReduceWork TaskResult TaskResult)
;;      (MapWork String)))
;; (define-type-alias PendingTask
;;   (Task TaskID Work))
;; (define-type-alias ConcreteTask
;;   (Task TaskID ConcreteWork))

#;(define (task-ready? [t : PendingTask] -> (Maybe ConcreteTask))
  (match t
    #;[(tuple $tid)
     ;; having to re-produce this is directly bc of no occurrence typing
     (some (task tid (map-work s)))]
    #;[(task $tid (reduce-work (right $v1)
                             (right $v2)))
     (some (task tid (reduce-work v1 v2)))]
    [_
     none]))

#;(cons (lambda () 0) (ann (list) (List (→fn Int))))

#;(Λ (X) (lambda ([x (Maybe X)]) (match x [none #f])))
#;(lambda ([x Int]) (match x [none #f]))
#;(match 1 [none #f])
#;(if #t 1 none)
#;none

#;(define (∀ (X) (unwrap! [x : (Maybe X)] -> Bool))
  #;(error "")
  (match x
    #;[(some (bind v X))
     v]
    [none
     #f
     #;(error "none")]))

#;(lambda ([tasks : (List (Maybe Int))])
  (define part (inst partition/either (Maybe Int) Int Int))
  (part tasks
        (lambda ([t : (Maybe Int)])
          (left 0)
          #;(match t
            [(some $ct)
             (right ct)]
            [none
             (left 0)]))))

#;(define (debug [tasks : (List Int)] -> (Tuple (List String) (List Int)))
  (define part (inst partition/either Int String Int))
  (part tasks
        (lambda ([t : Int])
          (left "hi"))))

(define (partition-ready-tasks [tasks : (List Int)]
                               -> (Tuple (List Int)
                                         (List Int)))
  (define part (inst partition/either Int Int Int))
  (part tasks
        (lambda ([t : Int])
          (right 0)
          #;(match (some 5)
              [(some $ct)
               (right ct)]
              [none
               (left 0)]))))


#;(define id : (∀ (X) (→fn X (List X) (List X)))
  (Λ (X) (lambda ([x X] [y (List X)]) (list x))))

#;(spawn (U)
         (start-facet echo
                      (on (message (tuple 1 1))
                          #f)))

#;(for/fold ([acc Int 0])
          ([x (list 1)])
  x)

#;(define-constructor* (left : Left v))

#;(define (f [x (Left Int)] -> Int)
    (define y x)
    (match y
    [(left (bind z Int))
     z]))

#;(#%app- expand/step #'(lambda ([x : Int])
    (define y x)
    y))

#;(lambda ([x : Int])
    (define y x)
    y)
#;(begin-for-syntax
  (define t #'(Maybe Unit))
  (define t- ((current-type-eval) t))
  (values #;displayln ((current-type?) t-))
  (define tt (syntax-parse (detach t- ':)
               [(#%plain-app x) #'x]))
  (pretty-print (syntax-debug-info tt)))

#;(begin-for-syntax
    (define t #'PendingTask)
    (define t- ((current-type-eval) t))
    (displayln ((current-type?) t-))
    )

#;(define t1 '(→ (Computation (Value ★/t)
                (Endpoints)
                (Roles (Branch (Effs (Realizes (TasksFinished- Symbol (Hash- String Int))))
                               (Effs (Branch (Effs (Realizes (TaskIsReady- Symbol (Task- (Tuple- Int Symbol) (U* (MapWork- String) (ReduceWork- (Hash- String Int) (Hash- String Int)))))))
                                             (Effs)))))
                (Spawns))
   (Tuple- Int Symbol)
   (Hash- String Int)))

#;(define t2 '(→ (Computation (Value ★/t)
                (Endpoints)
                (Roles (Branch (Effs (Realizes (TasksFinished- Symbol (Hash- String Int))))
                               (Effs (Branch (Effs (Realizes (TaskIsReady- Symbol (Task- (Tuple- Int Symbol) (U* (MapWork- String) (ReduceWork- (Hash- String Int) (Hash- String Int)))))))
                                             (Effs)))))
                (Spawns))
   (Tuple- Int Symbol)
   (Hash- String Int)))

#;(lambda ()
  (role-strings
  (start-facet x
    (define (push-results)
      (cond
        [(zero? 0)
         (start-facet done (assert #t))]
        [else
         #f]))
    (define (∀ (ρ) (perform-task [k : (proc -> ★/t #:roles (ρ))]))
      (start-facet perform
                   (on start (stop perform (k)))))
    (on start (call/inst perform-task push-results)))))
