#lang syndicate

(require (only-in racket/set
                  set
                  set-count
                  set-empty?
                  set-first
                  set-remove))
(require (only-in racket/list
                  partition
                  empty?
                  split-at))
(require (only-in racket/hash
                  hash-union))
(require (only-in racket/string
                  string-split))
(require (only-in racket/sequence
                  sequence->list))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; Logging

#;(define-logger flink)

(define (log fmt . args)
  (displayln (apply format fmt args))
  #;(log-message flink-logger 'warning #f (apply format fmt args)))

;; ---------------------------------------------------------------------------------------------------
;; TaskRunner


;; (Hash String Nat) String -> (Hash String Nat)
(define (word-count-increment h word)
  (hash-update h
               word
               add1
               (λ x 0)))

;; (Hash String Nat) (Listof String) -> (Hash String Nat)
(define (count-new-words word-count words)
  (for/fold ([result word-count])
            ([word words])
    (word-count-increment result word)))

(assertion-struct task-runner (id status))
(assertion-struct run-task (id task))
(assertion-struct task-execution-state (task state))
(assertion-struct task-input (task-id task input-id data))
(struct finished (data) #:transparent)
(struct executing (id) #:transparent)

(define IDLE 'idle)
(define RUNNING 'running)

(define (spawn-task-runner)
  (define id (gensym 'task-runner))
  (spawn #:name id
   (field [status IDLE])
   (assert (task-runner id (status)))
   (begin/dataflow
     (log "task-runner ~v state is: ~a" id (status)))
   ;; this only does map tasks atm
   (during (run-task id (task $tid $desc))
     (field [execution-state (if (equal? IDLE (status)) RUNNING OVERLOAD)]
            [word-count (hash)])
     ;; TODO - may need to include more correlation info in here to properly describe state when overloaded
     (assert (task-execution-state tid (execution-state)))
     ;; I think we have to avoid asking a non-idle runner to do anything
     (when (equal? IDLE (status))
       (on-stop (status IDLE)))
     (on-start
      (when (equal? IDLE (status))
        (status (executing tid))
        ;; since we currently finish everything in one turn, allow other actors to observe the changes in our
        ;; task-runner state by flushing pending actions.
        (flush!)
        (match desc
          [(map-task data)
           (word-count (count-new-words (word-count) data))
           (execution-state (finished (word-count)))
           #;(status IDLE)]
          [(reduce-task left right)
           (word-count (hash-union left right #:combine +))
           (execution-state (finished (word-count)))
           #;(status IDLE)])
        ;; don't think Jonathan has any examples w/ input streaming
        #;(on (asserted (task-input id task $chunk $words))
              ;; not currently worried about seeing the same chunk multiple times
              (cond
                [(null? words)
                 (execution-state (finished (word-count)))
                 ;; n.b. if we are asked to do multiple tasks at the same time this state update is not enough
                 (status IDLE)]
                [else (word-count (count-new-words (word-count) words))]))
        )))))

;; ---------------------------------------------------------------------------------------------------
;; TaskManager

(assertion-struct task-manager (id slots))
(assertion-struct submitted-task (manager task))
(assertion-struct job-manager-alive ())
(assertion-struct task-state (id desc))

;; task states
(define ACCEPTED 'accepted)
(define OVERLOAD 'overload)

(define (spawn-task-manager)
  (define id (gensym 'task-manager))
  (spawn #:name id
   (log "Task Manager (TM) ~a is running" id)
   (during (job-manager-alive)
     (log "TM learns about JM")
     ;; SUSPICION - these two query sets interfere with one another
     (define/query-set task-runners (task-runner $id _) id
       #:on-add (log "TM learns about task-runner ~a" id))
     ;; I wonder just how inefficient this is
     (define/query-set idle-runners (task-runner $id IDLE) id
       #:on-add (log "TM learns that task-runner ~a is IDLE" id)
       #:on-remove (log "TM learns that task-runner ~a is NOT IDLE" id))
     (assert (task-manager id (set-count (idle-runners))))
     (field [busy-runners (list)])
     (during (submitted-task id $t)
       (match-define (task task-id desc) t)
       #;(on-start (log "TM receives task ~a" task-id))
       (log "TM receives task ~a" task-id)
       (on-stop (log "TM finished with task ~a" task-id)
                (when (= task-id 6)
                  (log "TM idle-runners: ~a" (idle-runners))))
       (field [status ACCEPTED])
       (assert (task-state task-id (status)))
       (cond
         [(set-empty? (idle-runners))
          (log "TM can't run ~a right now" task-id)
          (status OVERLOAD)]
         [else
          (define runner (set-first (idle-runners)))
          ;; n.b. modifying a query set is questionable
          ;; but if we wait for the IDLE assertion to be retracted, we might assign multiple tasks to the same runner.
          ;; Could use the busy-runners field to avoid that
          (idle-runners (set-remove (idle-runners) runner))
          (log "TM assigns task ~a to runner ~a" task-id runner)
          (assert (run-task runner t))
          (status RUNNING)
          (on (asserted (task-execution-state task-id $state))
              (match state
                [(== RUNNING)
                 ;; nothing to do
                 (void)]
                [(finished results)
                 (log "TM receives the results of task ~a" task-id)
                 (status state)]
                [_
                 ;; TODO
                 ;; need input maybe?
                 #f]))])))))

;; ---------------------------------------------------------------------------------------------------
;; JobManager

(assertion-struct job (id tasks))
(assertion-struct job-finished (id data))
(struct job-description (id tasks) #:transparent)
(struct map-task (data) #:transparent)
(struct reduce-task (left right) #:transparent)
(struct task (id desc) #:transparent)

(define (spawn-job-manager)
  (spawn
    (assert (job-manager-alive))
    (log "Job Manager Up")
    ;; keep track of task managers, how many slots they say are open, and how many tasks we have assigned.
    (define/query-hash task-managers (task-manager $id $slots) id slots
      #:on-add (log "JM learns that ~a has ~v slots" id slots))
    (define (slots-available)
      (for/sum ([v (in-hash-values (task-managers))])
        v))
    ;; ID -> Void
    (define (take-slot! id)
      ;; make local changes to task-managers to reflect tasks delegated in the current turn
      (log "JM assigns a task to ~a" id)
      (task-managers (hash-update (task-managers) id sub1)))

    (during (job $job-id $tasks)
      (log "JM receives job ~a" job-id)
      (define-values (ready not-ready) (partition task-ready? tasks))
      (field [ready-tasks ready]
             [waiting-tasks not-ready]
             [tasks-in-progress 0]
             [data-partitions (hash)])

      (begin/dataflow
        (define slots (slots-available))
        (define-values (ts readys)
          (split-at/lenient (ready-tasks) slots))
        (for ([t ts])
          (perform-task t push-results))
        (unless (empty? ts)
          ;; the empty? check may be necessary to avoid a dataflow loop
          (ready-tasks readys)))

      ;; Task -> Void
      (define (add-ready-task! t)
        ;; TODO - use functional-queue.rkt from ../../
        (log "JM marks task ~a as ready" (task-id t))
        (ready-tasks (cons t (ready-tasks))))

      ;; need to parcel out tasks
      ;; Task (ID TaskResult -> Void) -> Void
      ;; Requires (task-ready? t)
      (define (perform-task t k)
        (react
         (on-start (tasks-in-progress (add1 (tasks-in-progress))))
         (on-stop (tasks-in-progress (sub1 (tasks-in-progress))))
         (match-define (task this-id desc) t)
         (log "JM begins on task ~a" this-id)

         (field [task-mngr #f])
         (begin/dataflow
           ;; n.b. cyclic data-flow dependency
           (unless (task-mngr)
             (define mngr
               (for/first ([(id slots) (in-hash (task-managers))]
                           #:unless (zero? slots))
                 id))
             (when mngr
               (take-slot! mngr)
               (task-mngr mngr))))
         ;; TODO - should respond if task manager dies
         (assert #:when (task-mngr)
                 (submitted-task (task-mngr) t))
         (on #:when (task-mngr)
             (asserted (task-state this-id $state))
             (match state
               [(== OVERLOAD)
                ;; need to find a new task manager
                ;; don't think we need a release-slot! here, because if we've heard back from a task manager,
                ;; they should have told us a different slot count since we tried to give them work
                (log "JM overloaded manager ~a with task ~a" (task-mngr) this-id)
                (task-mngr #f)]
               [(finished results)
                ;; TODO - guess-timation of what this should look like
                (log "JM receives the results of task ~a" this-id)
                (stop-current-facet (k this-id results))]
               [_
                ;; TODO - needs more data?
                #f]))))

      ;; ID Data -> Void
      ;; Update any dependent tasks with the results of the given task, moving
      ;; them to the ready queue when possible
      (define (push-results task-id data)
        (cond
          [(and (zero? (tasks-in-progress))
                (empty? (ready-tasks))
                (empty? (waiting-tasks)))
           ;; TODO - also need to ensure there are no tasks in progress
           (log "JM finished with job ~a" job-id)
           (react (assert (job-finished job-id data)))]
          [else
           ;; TODO - in MapReduce, there should be either 1 waiting task, or 0, meaning the job is done.
           (define still-waiting
             (for/fold ([ts '()])
                       ([t (in-list (waiting-tasks))])
               (define t+ (task+data t task-id data))
               (cond
                 [(task-ready? t+)
                  (add-ready-task! t+)
                  ts]
                 [else
                  (cons t+ ts)])))
           (waiting-tasks still-waiting)]))
      #f)))

;; Task -> Bool
;; Test if the task is ready to run
(define (task-ready? t)
  (match t
    [(task _ (reduce-task l r))
     (not (or (id? l) (id? r)))]
    [_ #t]))

;; Task Id Any -> Task
;; If the given task is waiting for this data, replace the waiting ID with the data
(define (task+data t id data)
  (match t
    [(task tid (reduce-task (== id) r))
     (task tid (reduce-task data r))]
    [(task tid (reduce-task l (== id)))
     (task tid (reduce-task l data))]
    [_ t]))

;; Any -> Bool
;; recognize ids
(define (id? x)
  (or (symbol? x) (exact-nonnegative-integer? x)))

;; (Listof A) Nat -> (Values (Listof A) (Listof A))
;; like split-at but allow a number larger than the length of the list
(define (split-at/lenient lst n)
  (split-at lst (min n (length lst))))

;; ---------------------------------------------------------------------------------------------------
;; Creating a Job

;; a WorkDesc is one of
;;   (map-task data)
;;   (reduce-task WorkDesc WorkDesc)

;; (Listof WordDesc) -> (Values (Listof WorkDesc) (Optionof WorkDesc))
;; Pair up elements of the input list into a list of reduce tasks, and if the input list is odd also
;; return the odd-one out
(define (pair-up ls)
  (let loop ([ls ls]
             [reductions '()])
    (match ls
      ['()
       (values reductions #f)]
      [(list x)
       (values reductions x)]
      [(list-rest x y more)
       (loop more (cons (reduce-task x y) reductions))])))


;; a TaskTree is one of
;;   (map-task data)
;;   (reduce-task TaskTree TaskTree)

;; (Listof String) -> TaskTree
;; Create a tree structure of tasks
(define (create-task-tree lines)
  (define map-tasks
    (for/list ([line (in-list lines)])
      ;; it may be more realistic to have the task runner do the split,
      ;; but this is how Jonathan's input looks
      (map-task (string-split line))))
  ;; build the tree up from the leaves
  (let loop ([nodes map-tasks])
    (match nodes
      ['()
       ;; input was empty
       (map-task "")]
      [(list x)
       x]
      [_
       (define-values (reductions left-over?)
         (pair-up nodes))
       (loop (if left-over?
                 (cons left-over? reductions)
                 reductions))])))

;; TaskTree -> (Listof Task)
;; flatten a task tree by assigning job-unique IDs
(define (task-tree->list tt)
  (define-values (tasks _)
    ;; TaskTree ID -> (Values (Listof Task) ID)
    ;; the input id is for the current node of the tree
    ;; returned id is the "next available" id, given ids are assigned in strict ascending order
    (let loop ([tt tt]
               [next-id 0])
      (match tt
        [(map-task _)
         (values (list (task next-id tt))
                       (add1 next-id))]
        [(reduce-task left right)
         (define left-id (add1 next-id))
         (define-values (lefts right-id)
           (loop left left-id))
         (define-values (rights next)
           (loop right right-id))
         (values (cons (task next-id (reduce-task left-id right-id))
                        (append lefts rights))
                 next)])))
  tasks)

;; InputPort -> Job
(define (create-job in)
  (define job-id (gensym 'job))
  (define input-lines (sequence->list (in-lines in)))
  (define tasks (task-tree->list (create-task-tree input-lines)))
  (job job-id tasks))

;; String -> Job
(define (string->job s)
  (create-job (open-input-string s)))

(module+ test
  (test-case
      "two-line job parsing"
    (define input "a b c\nd e f")
    (define j (string->job input))
    (check-true (job? j))
    (match-define (job jid tasks) j)
    (check-true (id? jid))
    (check-true (list? tasks))
    (check-true (andmap task? tasks))
    (match tasks
      [(list-no-order (task rid (reduce-task left right))
                      (task mid1 (map-task data1))
                      (task mid2 (map-task data2)))
       (check-true (id? left))
       (check-true (id? right))
       (check-equal? (set left right) (set mid1 mid2))
       (check-equal? (set (list "a" "b" "c") (list "d" "e" "f"))
                     (set data1 data2))]
      [_
       (displayln tasks)]))
  (test-case
      "empty input"
    (define input "")
    (define j (string->job input))
    (check-true (job? j))
    (match-define (job jid tasks) j)
    (check-true (id? jid))
    (check-true (list? tasks))
    (check-equal? (length tasks) 1)
    (check-equal? (task-desc (car tasks))
                  (map-task ""))))

;; ---------------------------------------------------------------------------------------------------
;; Client

;; Job -> Void
(define (spawn-client j)
  (spawn
   (assert j)
   (on (asserted (job-finished (job-id j) $data))
       (printf "job done!\n~a\n" data))))

;; ---------------------------------------------------------------------------------------------------
;; Main

(define input "a b c a b c\na b\n a b\na b")
(define j (string->job input))
;; expected:
;; #hash((a . 5) (b . 5) (c . 2))

(spawn-client j)
(spawn-job-manager)
(spawn-task-manager)
(spawn-task-runner)
(spawn-task-runner)
