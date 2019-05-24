#lang typed/syndicate/roles

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
(assertion-struct job-manager-alive : JobManagerAlive ())
#|
In turn, TaskManagers advertise their presence with (task-manager ID slots),
where ID is a unique id, and slots is a natural number. The number of slots
dictates how many tasks the TM can take on. To reduce contention, the JM
should only assign a task to a TM if the TM actually has the resources to
perform a task.
|#
(assertion-struct task-manager : TaskManager (id slots))
;; an ID is a symbol
(define-type-alias ID Symbol)
#|
The resources available to a TM are its associated TaskRunners (TRs). TaskRunners
assert their presence with (task-runner ID Status), where Status is one of
  - IDLE, when the TR is not executing a task
  - (executing TaskID), when the TR is executing the task with the given TaskID
  - OVERLOAD, when the TR has been asked to perform a task before it has
    finished its previous assignment. For the purposes of this model, it indicates a
    failure in the protocol; like the exchange between the JM and the TM, a TR
    should only receive tasks when it is IDLE.
|#
(assertion-struct task-runner : TaskRunner (id status))
(define-constructor* (executing : Executing id))
(define-type-alias TaskID Int)
(define-type-alias Status (U Symbol (Executing TaskID)))
(define IDLE : Status 'idle)
(define OVERLOAD : Status 'overload)

#|
Task Delegation Protocol
-----------------------

Task Delegation has two roles, TaskAssigner (TA) and TaskPerformer (TP).

A TaskAssigner asserts the association of a Task with a particular TaskPerformer
through
    (task-assignment ID ID Task)
where the first ID identifies the TP and the second identifies the job.
|#
(assertion-struct task-assignment : TaskAssignment (assignee job-id task))
#|
A Task is a (task TaskID Work), where Work is one of
  - (map-work String)
  - (reduce-work (U TaskID TaskResult) (U TaskID TaskResult)), referring to either the
    ID of the dependent task or its results. A reduce-work is ready to be executed
    when it has both results.

A TaskID is a natural number

A TaskResult is a (Hashof String Natural), counting the occurrences of words
|#
(require-struct task #:as Task #:from "flink-support.rkt")
(require-struct map-work #:as MapWork #:from "flink-support.rkt")
(require-struct reduce-work #:as ReduceWork #:from "flink-support.rkt")
(define-type-alias WordCount (Hash String Int))
(define-type-alias TaskResult WordCount)
(define-type-alias Reduce
  (ReduceWork (Either TaskID TaskResult)
              (Either TaskID TaskResult)))
(define-type-alias ReduceInput
  (ReduceWork TaskID TaskID))
(define-type-alias Work
  (U Reduce (MapWork String)))
(define-type-alias ConcreteWork
  (U (ReduceWork TaskResult TaskResult)
     (MapWork String)))
(define-type-alias InputTask
  (Task TaskID (U ReduceInput (MapWork String))))
(define-type-alias PendingTask
  (Task TaskID Work))
(define-type-alias ConcreteTask
  (Task TaskID ConcreteWork))
#|
The TaskPerformer responds to a task-assignment by describing its state with respect
to that task,
    (task-state ID ID ID TaskStateDesc)
where the first ID is that of the TP, the second is that of the job,
and the third that of the task.

A TaskStateDesc is one of
  - ACCEPTED, when the TP has the resources to perform the task. (TODO - not sure if this is ever visible, currently)
  - OVERLOAD/ts, when the TP does not have the resources to perform the task.
  - RUNNING, indicating that the task is being performed
  - (finished TaskResult), describing the results
|#
(assertion-struct task-state : TaskState (assignee job-id task-id desc))
(define-constructor* (finished : Finished data))
(define-type-alias TaskStateDesc
  (U Symbol (Finished TaskResult)))
(define ACCEPTED : TaskStateDesc 'accepted)
(define RUNNING : TaskStateDesc 'running)
;; this is gross, it's needed in part because equal? requires two of args of the same type
(define OVERLOAD/ts : TaskStateDesc 'overload)
#|
Two instances of the Task Delegation Protocol take place: one between the
JobManager and the TaskManager, and one between the TaskManager and its
TaskRunners.
|#

#|
Job Submission Protocol
-----------------------

Finally, Clients submit their jobs to the JobManager by asserting a Job, which is a (job ID (Listof Task)).
The JobManager then performs the job and, when finished, asserts (job-finished ID TaskResult)
|#
(require-struct job #:as Job #:from "flink-support.rkt")
(assertion-struct job-finished : JobFinished (id data))
(define-type-alias JobDesc (Job ID (List InputTask)))

(define-type-alias τc
  (U (TaskRunner ID Status)
     (TaskAssignment ID ID ConcreteTask)
     (Observe (TaskAssignment ID ★/t ★/t))
     (TaskState ID ID TaskID TaskStateDesc)
     (Observe (TaskState ID ID TaskID ★/t))
     (JobManagerAlive)
     (Observe (JobManagerAlive))
     (Observe (TaskRunner ★/t ★/t))
     (TaskManager ID Int)
     (Observe (TaskManager ★/t ★/t))
     JobDesc
     (Observe (Job ★/t ★/t))
     (JobFinished ID TaskResult)
     (Observe (JobFinished ID ★/t))))

;; ---------------------------------------------------------------------------------------------------
;; Util Macros

(require syntax/parse/define)

(define-simple-macro (log fmt . args)
  (begin
    (printf fmt . args)
    (printf "\n")))

;; ---------------------------------------------------------------------------------------------------
;; TaskRunner

(define (word-count-increment [h : WordCount]
                              [word : String]
                              -> WordCount)
  (hash-update/failure h
                       word
                       add1
                       0))

(define (count-new-words [word-count : WordCount]
                         [words : (List String)]
                         -> WordCount)
  (for/fold ([result word-count])
            ([word words])
    (word-count-increment result word)))

(require/typed "flink-support.rkt"
  [string->words : (→fn String (List String))])

(define (spawn-task-runner)
  (define id (gensym 'task-runner))
  (spawn τc
   (start-facet runner
    (field [status Status IDLE])
    (define (idle?) (equal? IDLE (ref status)))
    (assert (task-runner id (ref status)))
    (begin/dataflow
      (log "task-runner ~v state is: ~a" id (ref status)))
    (during (task-assignment id $job-id (task $task-id $desc))
      (field [state TaskStateDesc ACCEPTED])
      (assert (task-state id job-id task-id (ref state)))
      (cond
        [(idle?)
         ;; since we currently finish everything in one turn, these changes to status aren't
         ;; actually visible.
         (set! state RUNNING)
         (set! status (executing task-id))
         (match desc
           [(map-work $data)
            (define wc (count-new-words (ann (hash) WordCount) (string->words data)))
            (set! state (finished wc))]
           [(reduce-work $left $right)
            (define wc (hash-union/combine left right +))
            (set! state (finished wc))])
         (set! status IDLE)]
        [#t
         (set! status OVERLOAD)])))))

;; ---------------------------------------------------------------------------------------------------
;; TaskManager

(define (spawn-task-manager)
  (define id (gensym 'task-manager))
  (spawn τc
   (start-facet tm
    (log "Task Manager (TM) ~a is running" id)
    (during (job-manager-alive)
     (log "TM learns about JM")
     (define/query-set task-runners (task-runner $id _) id
       #;#:on-add #;(log "TM learns about task-runner ~a" id))
     ;; I wonder just how inefficient this is
     (define/query-set idle-runners (task-runner $id IDLE) id
       #;#:on-add #;(log "TM learns that task-runner ~a is IDLE" id)
       #;#:on-remove #;(log "TM learns that task-runner ~a is NOT IDLE" id))
     (assert (task-manager id (set-count (ref idle-runners))))
     (field [busy-runners (List ID) (list)])
     (define (can-accept?)
       (not (set-empty? (ref idle-runners))))
     (during (task-assignment id $job-id (task $task-id $desc))
       (define status0 : TaskStateDesc
         (if (can-accept?)
             RUNNING
             OVERLOAD/ts))
       (field [status TaskStateDesc status0])
       (assert (task-state id job-id task-id (ref status)))
       (when (can-accept?)
         (define runner (set-first (ref idle-runners)))
         ;; n.b. modifying a query set is questionable
         ;; but if we wait for the IDLE assertion to be retracted, we might assign multiple tasks to the same runner.
         ;; Could use the busy-runners field to avoid that
         (set! idle-runners (set-remove (ref idle-runners) runner))
         (log "TM assigns task ~a to runner ~a" task-id runner)
         ;; TODO - since we're both adding and removing from this set I'm not sure TRs
         ;; need to be making assertions about their idleness
         (on stop (set! idle-runners (set-add (ref idle-runners) runner)))
         (assert (task-assignment runner job-id (task task-id desc)))
         (on (asserted (task-state runner job-id task-id $st))
             (match st
               [ACCEPTED #f]
               [RUNNING #f]
               [OVERLOAD/ts
                (set! status OVERLOAD/ts)]
               [(finished discard)
                (set! status st)]))))))))

;; ---------------------------------------------------------------------------------------------------
;; JobManager

;; Task -> Bool
;; Test if the task is ready to run
(define (task-ready? [t : PendingTask] -> (Maybe ConcreteTask))
  (match t
    [(task $tid (map-work $s))
     ;; having to re-produce this is directly bc of no occurrence typing
     (some (task tid (map-work s)))]
    [(task $tid (reduce-work (right $v1)
                             (right $v2)))
     (some (task tid (reduce-work v1 v2)))]
    [_
     none]))

;; Task Id Any -> Task
;; If the given task is waiting for this data, replace the waiting ID with the data
(define (task+data [t : PendingTask]
                   [id : TaskID]
                   [data : TaskResult]
                   -> PendingTask)
  (match t
    [(task $tid (reduce-work (left id) $r))
     (task tid (reduce-work (right data) r))]
    [(task $tid (reduce-work $l (left id)))
     (task tid (reduce-work l (right data)))]
    [_ t]))


(require/typed "flink-support.rkt"
  [split-at/lenient- : (∀ (X) (→fn (List X) Int (List (List X))))])

(define (∀ (X) (split-at/lenient [xs : (List X)]
                                 [n : Int]
                                 -> (Tuple (List X) (List X))))
  (define l (split-at/lenient- xs n))
  (tuple (first l) (second l)))

(define (partition-ready-tasks [tasks : (List PendingTask)]
                               -> (Tuple (List PendingTask)
                                         (List ConcreteTask)))
  (define part (inst partition/either PendingTask PendingTask ConcreteTask))
  (part tasks
        (lambda ([t : PendingTask])
          (match (task-ready? t)
            [(some $ct)
             (right ct)]
            [none
             (left t)]))))

(define (input->pending-task [t : InputTask] -> PendingTask)
  (match t
    [(task $id (map-work $s))
     (task id (map-work s))]
    [(task $id (reduce-work $l $r))
     (task id (reduce-work (left l) (left r)))]))

(define (spawn-job-manager)
  (spawn τc
   (start-facet jm
    (assert (job-manager-alive))
    (log "Job Manager Up")

    ;; keep track of task managers, how many slots they say are open, and how many tasks we have assigned.
    (define/query-hash task-managers (task-manager $id $slots) id slots
      #;#:on-add #;(log "JM learns that ~a has ~v slots" id slots))

    ;; (Hashof TaskManagerID Nat)
    ;; to better understand the supply of slots for each task manager, keep track of the number
    ;; of requested tasks that we have yet to hear back about
    (field [requests-in-flight (Hash ID Int) (hash)])
    (define (slots-available)
      (for/sum ([(id v) (ref task-managers)])
        (max 0 (- v (hash-ref/failure (ref requests-in-flight) id 0)))))

    ;; ID -> Void
    ;; mark that we have requested the given task manager to perform a task
    (define (take-slot! [id : ID])
      (log "JM assigns a task to ~a" id)
      (set! requests-in-flight (hash-update/failure (ref requests-in-flight) id add1 0)))
    ;; ID -> Void
    ;; mark that we have heard back from the given manager about a requested task
    (define (received-answer! [id : ID])
      (set! requests-in-flight (hash-update (ref requests-in-flight) id sub1)))

    (during (job $job-id $tasks)
      (log "JM receives job ~a" job-id)
      (define pending (for/list ([t tasks])
                        (input->pending-task t)))
      (define-tuple (not-ready ready) (partition-ready-tasks pending #;(map input->pending-task tasks)))
      (field [ready-tasks (List ConcreteTask) ready]
             [waiting-tasks (List PendingTask) not-ready]
             [tasks-in-progress Int 0])

      ;; Task -> Void
      (define (add-ready-task! [t : ConcreteTask])
        ;; TODO - use functional-queue.rkt from ../../
        (match-define (task $tid _) t)
        (log "JM marks task ~a as ready" tid)
        (set! ready-tasks (cons t (ref ready-tasks))))

      ;; ID Data -> Void
      ;; Update any dependent tasks with the results of the given task, moving
      ;; them to the ready queue when possible
      (define (push-results [task-id : TaskID]
                            [data : TaskResult])
        (cond
          [(and (zero? (ref tasks-in-progress))
                (empty? (ref ready-tasks))
                (empty? (ref waiting-tasks)))
           (log "JM finished with job ~a" job-id)
           (start-facet done (assert (job-finished job-id data)))]
          [else
           ;; TODO - in MapReduce, there should be either 1 waiting task, or 0, meaning the job is done.
           (define still-waiting
             (for/fold ([ts : (List PendingTask) (list)])
                       ([t (ref waiting-tasks)])
               (define t+ (task+data t task-id data))
               (match (task-ready? t+)
                 [(some $ready)
                  (add-ready-task! ready)
                  ts]
                 [_
                  (cons t+ ts)])))
           (set! waiting-tasks still-waiting)]))

      ;; Task (ID TaskResult -> Void) -> Void
      ;; Requires (task-ready? t)
      (define (∀ (ρ) (perform-task [t : ConcreteTask]
                                   [k : (proc TaskID TaskResult -> ★/t
                                              #:roles (ρ))]))
        (start-facet perform
         (on start (set! tasks-in-progress (add1 (ref tasks-in-progress))))
         (on stop (set! tasks-in-progress (sub1 (ref tasks-in-progress))))
         (match-define (task $this-id $desc) t)
         (log "JM begins on task ~a" this-id)

         (define not-a-real-task-manager (gensym 'FAKE))
         (field [task-mngr ID not-a-real-task-manager])

         ;; ID -> ...
         (define (assign-task [mngr : ID])
           (start-facet this-facet
            (assert (task-assignment mngr job-id t))
            (on (retracted (task-manager mngr discard))
                ;; our task manager has crashed
                (stop this-facet
                      (set! task-mngr not-a-real-task-manager)))
            (on start
                ;; N.B. when this line was here, and not after `(when mngr ...)` above,
                ;; things didn't work. I think that due to script scheduling, all ready
                ;; tasks were being assigned to the manager
                #;(take-slot! mngr)
                (start-facet take-slot
                  (on (asserted (task-state mngr job-id this-id _))
                      (stop take-slot
                            (received-answer! mngr)))))
            (on (asserted (task-state mngr job-id this-id $status))
                (match status
                  [ACCEPTED #f]
                  [RUNNING #f]
                  [OVERLOAD/ts
                   ;; need to find a new task manager
                   ;; don't think we need a release-slot! here, because if we've heard back from a task manager,
                   ;; they should have told us a different slot count since we tried to give them work
                   (log "JM overloaded manager ~a with task ~a" mngr this-id)
                   (stop this-facet
                         (set! task-mngr not-a-real-task-manager))]
                  [(finished $results)
                   (log "JM receives the results of task ~a" this-id)
                   (stop perform (k this-id results))]))))

           (define (select-a-task-manager)
             (start-facet this-facet
              (begin/dataflow
                (when (equal? (ref task-mngr) not-a-real-task-manager)
                  (define mngr?
                    (for/first ([(id slots) (ref task-managers)]
                                #:when (positive? (- slots (hash-ref/failure (ref requests-in-flight) id 0))))
                      id))
                  (match mngr?
                    [(some $mngr)
                     (take-slot! mngr)
                     (set! task-mngr mngr)
                     (assign-task mngr)]
                    [none
                     #f])))))

         (on start (select-a-task-manager))))

      (begin/dataflow
        (define slots (slots-available))
        (define-tuple (ts readys)
          (split-at/lenient (ref ready-tasks) slots))
        (for ([t ts])
          (perform-task t push-results))
        (unless (empty? ts)
          ;; the empty? check may be necessary to avoid a dataflow loop
          (set! ready-tasks readys)))

      #f))))

;; ---------------------------------------------------------------------------------------------------
;; Client

;; Job -> Void
(define (spawn-client [j : JobDesc])
  (spawn τc
   (start-facet _
    (match-define (job $id _) j)
    (assert j)
    (on (asserted (job-finished id $data))
        (printf "job done!\n~a\n" data)))))

;; ---------------------------------------------------------------------------------------------------
;; Main

(require/typed "flink-support.rkt"
  [string->job : (→fn String JobDesc)]
  [file->job : (→fn String JobDesc)])

(define INPUT "a b c a b c\na b\n a b\na b")
;; expected:
;; #hash((a . 5) (b . 5) (c . 2))

(run-ground-dataspace τc
  (spawn-job-manager)
  (spawn-task-manager)
  (spawn-task-runner)
  (spawn-task-runner)
  (spawn-client (string->job INPUT)))
