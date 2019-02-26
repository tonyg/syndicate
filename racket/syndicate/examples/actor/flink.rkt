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
                  string-split
                  string-trim))
(require (only-in racket/sequence
                  sequence->list))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; Protocol

#|
Conversations in the flink dataspace primarily concern two topics: presence and
task execution.

Presence Protocol
-----------------

The JobManager (JM) asserts its presence with (job-manager-alive). The operation
of each TaskManager (TM) is contingent on the presence of a job manager.
|#
(assertion-struct job-manager-alive ())
#|
In turn, TaskManagers advertise their presence with (task-manager ID slots),
where ID is a unique id, and slots is a natural number. The number of slots
dictates how many tasks the TM can take on. To reduce contention, we the JM
should only assign a task to a TM if the TM actually has the resources to
perform a task.
|#
(assertion-struct task-manager (id slots))
;; an ID is a symbol or a natural number.
;; Any -> Bool
;; recognize ids
(define (id? x)
  (or (symbol? x) (exact-nonnegative-integer? x)))
#|
The resources available to a TM are its associated TaskRunners (TRs). TaskRunners
assert their presence with (task-runner ID Status), where Status is one of
  - IDLE, when the TR is not executing a task
  - (executing ID), when the TR is executing the task with the given ID
  - OVERLOAD, when the TR has been asked to perform a task before it has
    finished its previous assignment. For the purposes of this model, it indicates a
    failure in the protocol; like the exchange between the JM and the TM, a TR
    should only receive tasks when it is IDLE.
|#
(assertion-struct task-runner (id status))
(define IDLE 'idle)
(define OVERLOAD 'overload)
(struct executing (id) #:transparent)

#|
Task Execution Protocol
-----------------------

When the JobManager receives a Job, it assigns its constituent Tasks to the
TaskManagers, subject to TM availability and the readiness of each Task.

The JM asserts the association of a Task with a particular TM
through (submitted-task ID Task), where ID identifies the TM.
TODO - also need to correlate Job ID through here.
|#
(assertion-struct submitted-task (manager task))
#|
A Task is a (task ID Work), where Work is one of
  - (map-task String)
  - (reduce-task (U ID TaskResult) (U ID TaskResult)), referring to either the
    ID of the dependent task or its results. A reduce-task is ready to be executed
    when it has both results.

A TaskResult is a (Hashof String Natural), counting the occurrences of words
|#
(struct task (id desc) #:transparent)
(struct map-task (data) #:transparent)
(struct reduce-task (left right) #:transparent)

#|
A TaskManager responds to a submitted-task by describing its state with respect
to that task, (task-state ID TaskStateDesc), where ID is that of the
submitted-task (TODO - that doesn't seem like enough). TaskStateDesc is one of
  - ACCEPTED, when the TM can assign the Task to an available JR (TODO - not sure if this is ever visible, currently)
  - OVERLOAD, when the TM does not have the resources to perform the task.
  - RUNNING, indicating that the task has successfully been delegated to a JR
  - (finished TaskResult), describing the results
|#
(assertion-struct task-state (id desc))
(struct finished (data) #:transparent)
(define ACCEPTED 'accepted)
(define RUNNING 'running)
#|
Upon receipt, a TaskManager selects a TaskRunner to perform it. The TM asserts
the association, (run-task ID Task), where ID is that of the given TaskRunner.

The TaskRunner shares its reaction to a task assignment with an assertion,
(task-execution-state Task ExecutionState). (TODO - the TR ID should be in there!).
The ExecutionState is one of
  - RUNNING, indicating that the TR has accepted and is executing the task
  - OVERLOAD, when the TR is overloaded
  - (finished TaskResult), describing the results
TODO - merge this with TaskStateDesc
TODO TODO - merge the JR/TM and TM/TR task protocols with one another - TODO TODO
|#
(assertion-struct run-task (id task))
(assertion-struct task-execution-state (task state))

#|
Job Submission Protocol
-----------------------

Finally, Clients submit their jobs to the JobManager by asserting a Job, which is a (job ID (Listof Task)).
The JobManager then performs the job and, when finished, asserts (job-finished ID TaskResult)
|#
(assertion-struct job (id tasks))
(assertion-struct job-finished (id data))

;; ---------------------------------------------------------------------------------------------------
;; Logging

(define (log fmt . args)
  (displayln (apply format fmt args)))

;; ---------------------------------------------------------------------------------------------------
;; TaskRunner

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
           (word-count (count-new-words (word-count) (string->words data)))
           (execution-state (finished (word-count)))]
          [(reduce-task left right)
           (word-count (hash-union left right #:combine +))
           (execution-state (finished (word-count)))]))))))

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

;; String -> (Listof String)
;; Return the white space-separated words, trimming off leading & trailing punctuation
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

;; ---------------------------------------------------------------------------------------------------
;; TaskManager

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

(define (spawn-job-manager)
  (spawn
    (assert (job-manager-alive))
    (log "Job Manager Up")

    ;; keep track of task managers, how many slots they say are open, and how many tasks we have assigned.
    (define/query-hash task-managers (task-manager $id $slots) id slots
      #:on-add (log "JM learns that ~a has ~v slots" id slots))

    ;; (Hashof TaskManagerID Nat)
    ;; to better understand the supply of slots for each task manager, keep track of the number
    ;; of requested tasks that we have yet to hear back about
    (field [requests-in-flight (hash)])
    (define (slots-available)
      (for/sum ([(id v) (in-hash (task-managers))])
        (max 0 (- v (hash-ref (requests-in-flight) id 0)))))
    ;; ID -> Void
    ;; mark that we have requested the given task manager to perform a task
    (define (take-slot! id)
      (log "JM assigns a task to ~a" id)
      (requests-in-flight (hash-update (requests-in-flight) id add1 0)))
    ;; ID -> Void
    ;; mark that we have heard back from the given manager about a requested task
    (define (received-answer! id)
      (requests-in-flight (hash-update (requests-in-flight) id sub1)))

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
                           #:when (positive? (- slots (hash-ref (requests-in-flight) id 0))))
                 id))
             (when mngr
               (take-slot! mngr)
               (react (stop-when (asserted (task-state this-id _))
                                 (received-answer! mngr)))
               (task-mngr mngr))))
         ;; TODO - should respond if task manager dies
         (assert #:when (task-mngr)
                 (submitted-task (task-mngr) t))
         (on #:when (task-mngr)
             (asserted (task-state this-id $state))
             (match state
               [(== ACCEPTED)
                #f]
               [(== RUNNING)
                ;; nothing to do
                #f]
               [(== OVERLOAD)
                ;; need to find a new task manager
                ;; don't think we need a release-slot! here, because if we've heard back from a task manager,
                ;; they should have told us a different slot count since we tried to give them work
                (log "JM overloaded manager ~a with task ~a" (task-mngr) this-id)
                (task-mngr #f)]
               [(finished results)
                ;; TODO - guess-timation of what this should look like
                (log "JM receives the results of task ~a" this-id)
                (stop-current-facet (k this-id results))]))))

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


;; (Listof A) Nat -> (Values (Listof A) (Listof A))
;; like split-at but allow a number larger than the length of the list
(define (split-at/lenient lst n)
  (split-at lst (min n (length lst))))

;; ---------------------------------------------------------------------------------------------------
;; Client

;; Job -> Void
(define (spawn-client j)
  (spawn
   (assert j)
   (on (asserted (job-finished (job-id j) $data))
       (printf "job done!\n~a\n" data))))

;; ---------------------------------------------------------------------------------------------------
;; Observe interaction between task and job manager

(define (spawn-observer)
  (spawn
   (during (job-manager-alive)
     (during (task-manager $tm-id _)
       (define/query-set requests (submitted-task tm-id (task $tid _)) tid)
       (field [high-water-mark 0])
       (on (asserted (task-manager tm-id $slots))
           (when (> slots (high-water-mark))
             (high-water-mark slots)))
       (begin/dataflow
         (when (> (set-count (requests)) (high-water-mark))
           (log "!! DEMAND > SUPPLY !!")))))))

;; ---------------------------------------------------------------------------------------------------
;; Creating a Job

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
       (loop more (cons (reduce-task x y) reductions))])))


;; a TaskTree is one of
;;   (map-task data)
;;   (reduce-task TaskTree TaskTree)

;; (Listof String) -> TaskTree
;; Create a tree structure of tasks
(define (create-task-tree lines)
  (define map-tasks
    (for/list ([line (in-list lines)])
      (map-task line)))
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

;; PathString -> Job
(define (file->job path)
  (define in (open-input-file path))
  (define j (create-job in))
  (close-input-port in)
  j)

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
       (check-equal? (set data1 data2)
                     (set "a b c" "d e f"))]
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
;; Main

(define input "a b c a b c\na b\n a b\na b")
(define j (string->job input))
;; expected:
;; #hash((a . 5) (b . 5) (c . 2))

(spawn-client (file->job "lorem.txt"))
(spawn-job-manager)
(spawn-task-manager)
(spawn-task-runner)
(spawn-task-runner)
(spawn-observer)
