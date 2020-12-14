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
assert their presence with (task-runner ID),
|#
(assertion-struct task-runner : TaskRunner (id))

#|
A Status is one of
- IDLE, when the TR is not executing a task
- (executing Int), when the TR is executing the task with identified by the Int
- OVERLOAD, when the TR has been asked to perform a task before it has
finished its previous assignment. For the purposes of this model, it indicates a
failure in the protocol; like the exchange between the JM and the TM, a TR
should only receive tasks when it is IDLE.
|#
(define-constructor* (executing : Executing id))
(define-type-alias Status (U Symbol (Executing Int)))
(define IDLE : Status 'idle)
(define OVERLOAD : Status 'overload)

#|
Task Delegation Protocol
-----------------------

Task Delegation has two roles, TaskAssigner (TA) and TaskPerformer (TP).

A TaskAssigner requests the performance of a Task with a particular TaskPerformer
through the assertion of interest
(observe (task-performance ID Task ★))
where the ID identifies the TP
|#
(assertion-struct task-performance : TaskPerformance (assignee task desc))
#|
A Task is a (task TaskID Work), where Work is one of
  - (map-work String)
  - (reduce-work (U Int TaskResult) (U Int TaskResult)), referring to either the
    ID of the dependent task or its results. A reduce-work is ready to be executed
    when it has both results.

A TaskID is a (Tuple Int ID), where the first Int is specific to the individual
task and the second identifies the job it belongs to.

A TaskResult is a (Hashof String Natural), counting the occurrences of words
|#
(require-struct task #:as Task #:from "flink-support.rkt")
(require-struct map-work #:as MapWork #:from "flink-support.rkt")
(require-struct reduce-work #:as ReduceWork #:from "flink-support.rkt")
(define-type-alias TaskID (Tuple Int ID))
(define-type-alias WordCount (Hash String Int))
(define-type-alias TaskResult WordCount)
(define-type-alias Reduce
  (ReduceWork (Either Int TaskResult)
              (Either Int TaskResult)))
(define-type-alias ReduceInput
  (ReduceWork Int Int))
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
The TaskPerformer responds to a request by describing its state with respect
to that task,
(task-performance ID Task TaskStateDesc)

A TaskStateDesc is one of
  - ACCEPTED, when the TP has the resources to perform the task. (TODO - not sure if this is ever visible, currently)
  - OVERLOAD/ts, when the TP does not have the resources to perform the task.
  - RUNNING, indicating that the task is being performed
  - (finished TaskResult), describing the results
|#
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

;; I think this is wrong by explicitly requiring that the facet stop in response
(define-type-alias TaskAssigner-v1
  (Role (assign)
    (Shares (Observe (TaskPerformance ID ConcreteTask ★/t)))
    ;; would be nice to say how the TaskIDs relate to each other
    (Reacts (Asserted (TaskPerformance ID ConcreteTask ★/t))
            (Branch (Stop assign)
                    (Effs)))))

(define-type-alias TaskAssigner
  (Role (assign)
    ;; would be nice to say how the TaskIDs relate to each other
    (Reacts (Asserted (TaskPerformance ID ConcreteTask TaskStateDesc))
            )))

(export-type "task-assigner.rktd" TaskAssigner)

(define-type-alias TaskPerformer
  (Role (listen)
    (During (Observe (TaskPerformance ID ConcreteTask ★/t))
      ;; would be nice to say how the IDs and TaskIDs relate to each other
      ;; BUG in spec; ConcreteTask used to be just TaskID (when I streamlined protocol)
      (Shares (TaskPerformance ID ConcreteTask TaskStateDesc)))))

#|
Job Submission Protocol
-----------------------

Finally, Clients submit their jobs to the JobManager by asserting interest
    (observe (job-completion ID (Listof Task) ★))

The JobManager then performs the job and, when finished, asserts
    (job-completion ID (Listof Task) TaskResult)

|#
(require-struct job #:as Job #:from "flink-support.rkt")
(assertion-struct job-completion : JobCompletion (id data result))
(define-type-alias JobDesc (Job ID (List InputTask)))

(define-type-alias τc
  (U (TaskRunner ID)
     (Observe (TaskPerformance ID ConcreteTask ★/t))
     (TaskPerformance ID ConcreteTask TaskStateDesc)
     (Observe (Observe (TaskPerformance ID ★/t ★/t)))
     (JobManagerAlive)
     (Observe (JobManagerAlive))
     (Observe (TaskRunner ★/t))
     (TaskManager ID Int)
     (Observe (TaskManager ★/t ★/t))
     (JobCompletion ID (List InputTask) TaskResult)
     (Observe (JobCompletion ID (List InputTask) ★/t))
     (Observe (Observe (JobCompletion ★/t ★/t ★/t)))))

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

(define (spawn-task-runner [id : ID] [tm-id : ID])
  (spawn τc
   (lift+define-role task-runner-impl
   (start-facet runner ;; #:includes-behavior TaskPerformer
    (assert (task-runner id))
    (on (retracted (task-manager tm-id _))
        (stop runner))
    (during (observe (task-performance id $t _))
      (match-define (task $task-id $desc) t)
      (field [state TaskStateDesc ACCEPTED])
      (assert (task-performance id t (ref state)))
      ;; since we currently finish everything in one turn, these changes to status aren't
      ;; actually visible.
      (set! state RUNNING)
      (match desc
        [(map-work $data)
         (define wc (count-new-words (ann (hash) WordCount) (string->words data)))
         (set! state (finished wc))]
        [(reduce-work $left $right)
         (define wc (hash-union/combine left right +))
         (set! state (finished wc))]))))))

;; ---------------------------------------------------------------------------------------------------
;; TaskManager


(define (spawn-task-manager [num-task-runners : Int])
  (define id (gensym 'task-manager))
  (spawn τc
   (#;begin  lift+define-role task-manager-impl ;;export-roles "task-manager-impl.rktd"
   (start-facet tm ;; #:includes-behavior TaskAssigner
    (log "Task Manager (TM) ~a is running" id)
    (during (job-manager-alive)
     (log "TM ~a learns about JM" id)

     (field [task-runners (Set ID) (set)])

     (on start
       (for ([_ (in-range num-task-runners)])
         (define tr-id (gensym 'task-runner))
         (start-facet monitor-task-runner
           (on start (spawn-task-runner tr-id id))
           (on (asserted (task-runner tr-id))
               (log "TM ~a successfully created task-runner ~a" id tr-id)
               (set! task-runners (set-add (ref task-runners) tr-id)))
           (on (retracted (task-runner tr-id))
               (log "TM ~a detected failure of task runner ~a, restarting" id tr-id)
               (set! task-runners (set-remove (ref task-runners) tr-id))
               (spawn-task-runner tr-id id)))))


     (field [busy-runners (Set ID) (set)])

     (define/dataflow idle-runners
       (set-count (set-subtract (ref task-runners) (ref busy-runners))))

     (assert (task-manager id (ref idle-runners)))

     (define (can-accept?)
       (positive? (ref idle-runners)))

     (define (select-runner)
       (define runner (for/first ([r (in-set (ref task-runners))]
                                  #:unless (set-member? (ref busy-runners) r))
                        r))
       (match runner
         [(some $r)
          (set! busy-runners (set-add (ref busy-runners) r))
          r]
         [none
          (error "need to call can-accept? before selecting a runner")]))

     (during (observe (task-performance id $t _))
       (match-define (task $task-id $desc) t)
       (define status0 : TaskStateDesc
         (if (can-accept?)
             RUNNING
             OVERLOAD/ts))
       (field [status TaskStateDesc status0])
       (assert (task-performance id t (ref status)))
       (when (can-accept?)
         (define runner (select-runner))
         (log "TM ~a assigns task ~a to runner ~a" id task-id runner)
         (on stop (set! busy-runners (set-remove (ref busy-runners) runner)))
         (on (asserted (task-performance runner t $st))
             (match st
               [ACCEPTED #f]
               [RUNNING #f]
               [OVERLOAD/ts
                (set! status OVERLOAD/ts)]
               [(finished discard)
                (set! status st)])))))))))

;; ---------------------------------------------------------------------------------------------------
;; JobManager

;; Task Int Any -> Task
;; If the given task is waiting for this data, replace the waiting ID with the data
(define (task+data [t : PendingTask]
                   [id : Int]
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
     ;; with occurrence typing, could just return t
     (task id (map-work s))]
    [(task $id (reduce-work $l $r))
     (task id (reduce-work (left l) (left r)))]))


(message-struct tasks-finished : TasksFinished (id results))

;; assertions used for internal slot-management protocol
(assertion-struct slots : Slots (v))
(assertion-struct slot-assignment : SlotAssignment (who mngr))
;; tid is the TaskID, rid is a unique symbol to a particular request for a slot
(define-constructor* (request-id : ReqID tid rid))
(define-type-alias RequestID (ReqID TaskID ID))
(message-struct task-is-ready : TaskIsReady (job-id task))

(define (spawn-job-manager)
  (spawn τc
    (lift+define-role job-manager-impl ;; export-roles "job-manager-impl.rktd"
    (start-facet jm ;; #:includes-behavior TaskAssigner
    (assert (job-manager-alive))
    (log "Job Manager Up")

    (on start
      (start-facet slot-manager
        ;; keep track of task managers, how many slots they say are open, and how many tasks we have assigned.
        (define/query-hash task-managers (task-manager $id:ID $slots:Int) id slots
          #:on-add (log "JM learns that ~a has ~v slots" id (hash-ref (ref task-managers) id)))

        (field ;; how many outstanding assignments there are for each task manager
               [requests-in-flight (Hash ID Int) (hash)]
               ;; map a request's ID to the manager it is assigned to
               [assignments (Hash ID ID) (hash)])
        (define (slots-available)
          (for/sum ([(id v) (ref task-managers)])
            (max 0 (- v (hash-ref/failure (ref requests-in-flight) id 0)))))

        (define (try-take-slot! [me : ID] -> (Maybe ID))
          (define mngr?
            (for/first ([(id slots) (ref task-managers)]
                        #:when (positive? (- slots (hash-ref/failure (ref requests-in-flight) id 0))))
              id))
          (match mngr?
            [(some $m)
             (set! assignments (hash-set (ref assignments) me m))
             (set! requests-in-flight (hash-update/failure (ref requests-in-flight) m add1 0))]
            [none
             #f])
          mngr?)

        (know (slots (slots-available)))

        (during (know (observe (slot-assignment (request-id $tid:TaskID $who:ID) _)))
          (on start
           (start-facet assign-manager
            ;; what if one manager gains a slot but another loses one, so n stays the same?
            (on (know (slots $n:Int))
              #;(log "Dispatcher request ~a learns there are ~a slots" tid n)
              (unless (or (zero? n) (hash-has-key? (ref assignments) who))
                (define mngr? (try-take-slot! who))
                (match mngr?
                  [(some $mngr)
                  (stop assign-manager
                   (log "Dispatcher assigns task ~a to ~a" tid mngr)
                   (start-facet _ (know (slot-assignment (request-id tid who) mngr)))
                   (start-facet waiting-for-answer
                    (on (asserted (observe (task-performance mngr (task tid $x) _)))
                        (start-facet _ (on (asserted (task-performance mngr (task tid x) _))
                                           (log "Dispatcher sees answer for ~a" tid)
                                           (stop waiting-for-answer))))
                    (on stop
                     (set! requests-in-flight (hash-update (ref requests-in-flight) mngr sub1)))))]
                  [_ #f])))))
          (on stop (set! assignments (hash-remove (ref assignments) who))))))

    (during (observe (job-completion $job-id $tasks _))
      (log "JM receives job ~a" job-id)
      (define pending (for/list ([t tasks])
                        (input->pending-task t)))
      (define-tuple (not-ready ready) (partition-ready-tasks pending))
      (field [waiting-tasks (List PendingTask) not-ready]
             [tasks-in-progress Int 0])

      ;; Task -> Void
      (define (add-ready-task! [t : ConcreteTask])
        ;; TODO - use functional-queue.rkt from ../../
        (match-define (task $tid _) t)
        (log "JM marks task ~a as ready" tid)
        (realize! (task-is-ready job-id t)))

      ;; ID Data -> Void
      ;; Update any dependent tasks with the results of the given task, moving
      ;; them to the ready queue when possible
      (define (push-results [task-id : TaskID]
                            [data : TaskResult])
        (cond
          [(and (zero? (ref tasks-in-progress))
                (empty? (ref waiting-tasks)))
           (log "JM finished with job ~a" job-id)
           (realize! (tasks-finished job-id data))]
          [else
           ;; TODO - in MapReduce, there should be either 1 waiting task, or 0, meaning the job is done.
           (define still-waiting
             (for/fold ([ts : (List PendingTask) (list)])
                       ([t (ref waiting-tasks)])
               (define t+ (task+data t (select 0 task-id) data))
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

         ;; ID -> ...
         (define (assign-task [mngr : ID]
                              [request-again! : (→fn ★/t)])
           (start-facet assign
            (on (retracted (task-manager mngr _))
                ;; our task manager has crashed
                (stop assign (request-again!)))
            (on (asserted (task-performance mngr t $status))
                (match status
                  [ACCEPTED #f]
                  [RUNNING #f]
                  [OVERLOAD/ts
                   ;; need to find a new task manager
                   ;; don't think we need a release-slot! here, because if we've heard back from a task manager,
                   ;; they should have told us a different slot count since we tried to give them work
                   (log "JM overloaded manager ~a with task ~a" mngr this-id)
                   (stop assign (request-again!))]
                  [(finished $results)
                   (log "JM receives the results of task ~a" this-id)
                   (stop perform (k this-id results))]))))

           (define (select-a-task-manager)
             (start-facet select
               (field [req-id ID (gensym 'perform-task)])
               (define (request-again!) (set! req-id (gensym 'perform-task)))
               (on (know (slot-assignment (request-id this-id (ref req-id)) $mngr:ID))
                   (assign-task mngr request-again!))))

         (on start (select-a-task-manager))))

      (on start
       (start-facet delegate-tasks
         (on (realize (tasks-finished job-id $data:TaskResult))
             (stop delegate-tasks
                   (start-facet done (assert (job-completion job-id tasks data)))))
         (on (realize (task-is-ready job-id $t:ConcreteTask))
             (perform-task t push-results)))
       (for ([t (in-list ready)])
         (add-ready-task! t))))))))

;; ---------------------------------------------------------------------------------------------------
;; Client

;; Job -> Void
(define (spawn-client [j : JobDesc])
  (spawn τc
   (start-facet _
    (match-define (job $id $tasks) j)
    (on (asserted (job-completion id tasks $data))
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
  (spawn-task-manager 2)
  (spawn-task-manager 3)
  (spawn-client (file->job "lorem.txt"))
  (spawn-client (string->job INPUT)))

(module+ test
  (check-simulates task-runner-impl task-runner-impl)
  (check-has-simulating-subgraph task-runner-impl TaskPerformer)
  (check-simulates task-manager-impl task-manager-impl)
  (check-has-simulating-subgraph task-manager-impl TaskPerformer)
  (check-has-simulating-subgraph task-manager-impl TaskAssigner)
  (check-has-simulating-subgraph job-manager-impl TaskAssigner))
