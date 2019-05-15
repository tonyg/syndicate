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
(define-constructor* (task : Task id desc))
(define-constructor* (map-work : MapWork data))
(define-constructor* (reduce-work : ReduceWork left right))
(define-type-alias WordCount (Hash String Int))
(define-type-alias TaskResult WordCount)
(define-type-alias Reduce
  (ReduceWork (U TaskID TaskResult)
              (U TaskID TaskResult)))
(define-type-alias Work
  (U Reduce (MapWork String)))
(define-type-alias ConcreteWork
  (U (ReduceWork TaskResult TaskResult)
     (MapWork String)))
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
  - OVERLOAD, when the TP does not have the resources to perform the task.
  - RUNNING, indicating that the task is being performed
  - (finished TaskResult), describing the results
|#
(assertion-struct task-state : TaskState (assignee job-id task-id desc))
(define-constructor* (finished : Finished data))
(define-type-alias TaskStateDesc
  (U Symbol (Finished TaskResult)))
(define ACCEPTED : TaskStateDesc 'accepted)
(define RUNNING : TaskStateDesc 'running)
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
(assertion-struct job : Job (id tasks))
(assertion-struct job-finished : JobFinished (id data))

(define-type-alias τc
  (U (TaskRunner ID Status)
     (TaskAssignment ID ID ConcreteTask)
     (Observe (TaskAssignment ID ★/t ★/t))
     (TaskState ID ID TaskID TaskStateDesc)
     ))

;; ---------------------------------------------------------------------------------------------------
;; TaskRunner

(define (word-count-increment [h : WordCount]
                              [word : String]
                              -> WordCount)
  (hash-update/failure h
                       word
                       add1
                       (lambda () 0)))

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
      (printf "task-runner ~v state is: ~a" id (ref status)))
    (during (task-assignment id
                             (bind job-id ID)
                             (task (bind task-id TaskID)
                                   (bind desc ConcreteWork)))
      (field [state TaskStateDesc ACCEPTED])
      (assert (task-state id job-id task-id (ref state)))
      (cond
        [(idle?)
         ;; since we currently finish everything in one turn, these changes to status aren't
         ;; actually visible.
         (set! state RUNNING)
         (set! status (executing task-id))
         (match desc
           [(map-work (bind data String))
            (define wc (count-new-words (ann (hash) WordCount) (string->words data)))
            (set! state (finished wc))]
           [(reduce-work (bind left WordCount) (bind right WordCount))
            (define wc (hash-union/combine left right +))
            (set! state (finished wc))])
         (set! status IDLE)]
        [#t
         (set! status OVERLOAD)])))))
