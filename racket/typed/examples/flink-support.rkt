#lang racket

(provide string->words
         split-at/lenient-
         (struct-out job)
         (struct-out task)
         (struct-out map-work)
         (struct-out reduce-work)
         string->job
         file->job)

(require (only-in racket/list
                  split-at))
(module+ test
  (require rackunit))

(define (string->words s)
    (map (lambda (w) (string-trim w #px"\\p{P}")) (string-split s)))

(module+ test
  (check-equal? (string->words "good day sir")
                (list "good" "day" "sir"))
  (check-equal? (string->words "")
                (list))
  (check-equal? (string->words "good eve ma'am")
                (list "good" "eve" "ma'am"))
  (check-equal? (string->words "please sir. may I have another?")
                (list "please" "sir" "may" "I" "have" "another"))
  ;; TODO - currently fails
  #;(check-equal? (string->words "but wait---there's more")
                  (list "but" "wait" "there's" "more")))

;; (Listof A) Nat -> (List (Listof A) (Listof A))
;; like split-at but allow a number larger than the length of the list
(define (split-at/lenient- lst n)
  (define-values (a b)
    (split-at lst (min n (length lst))))
  (list a b))

;; ---------------------------------------------------------------------------------------------------
;; Creating a Job

(struct job (id tasks) #:transparent)
(struct task (id desc) #:transparent)
(struct map-work (data) #:transparent)
(struct reduce-work (left right) #:transparent)

;; (Listof WorkDesc) -> (Values (Listof WorkDesc) (Optionof WorkDesc))
;; Pair up elements of the input list into a list of reduce tasks, and if the input list is odd also
;; return the odd-one out.
;; Conceptually, it does something like this:
;;   '(a b c d) => '((a b) (c d))
;;   '(a b c d e) => '((a b) (c d) e)
(define (pair-up ls)
  (let loop ([ls ls]
             [reductions '()])
    (match ls
      ['()
       (values reductions #f)]
      [(list x)
       (values reductions x)]
      [(list-rest x y more)
       (loop more (cons (reduce-work x y) reductions))])))


;; a TaskTree is one of
;;   (map-work data)
;;   (reduce-work TaskTree TaskTree)

;; (Listof String) -> TaskTree
;; Create a tree structure of tasks
(define (create-task-tree lines)
  (define map-works
    (for/list ([line (in-list lines)])
      (map-work line)))
  ;; build the tree up from the leaves
  (let loop ([nodes map-works])
    (match nodes
      ['()
       ;; input was empty
       (map-work "")]
      [(list x)
       x]
      [_
       (define-values (reductions left-over?)
         (pair-up nodes))
       (loop (if left-over?
                 (cons left-over? reductions)
                 reductions))])))

;; TaskTree ID -> (Listof Task)
;; flatten a task tree by assigning job-unique IDs
(define (task-tree->list tt job-id)
  (define-values (tasks _)
    ;; TaskTree ID -> (Values (Listof Task) ID)
    ;; the input id is for the current node of the tree
    ;; returned id is the "next available" id, given ids are assigned in strict ascending order
    (let loop ([tt tt]
               [next-id 0])
      (match tt
        [(map-work _)
         ;; NOTE : utilizing knowledge of Tuple representation here
         (values (list (task (list 'tuple next-id job-id) tt))
                       (add1 next-id))]
        [(reduce-work left right)
         (define left-id (add1 next-id))
         (define-values (lefts right-id)
           (loop left left-id))
         (define-values (rights next)
           (loop right right-id))
         (values (cons (task (list 'tuple next-id job-id) (reduce-work left-id right-id))
                        (append lefts rights))
                 next)])))
  tasks)

;; InputPort -> Job
(define (create-job in)
  (define job-id (gensym 'job))
  (define input-lines (sequence->list (in-lines in)))
  (define tasks (task-tree->list (create-task-tree input-lines) job-id))
  (job job-id tasks))

;; String -> Job
(define (string->job s)
  (create-job (open-input-string s)))

;; PathString -> Job
(define (file->job path)
  (define in (open-input-file path))
  (define j (create-job in))
  (close-input-port in)
  j)
