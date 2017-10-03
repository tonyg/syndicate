#lang syndicate/core
;; Timer driver.

;; Uses mutable state internally, but because the scope of the
;; mutation is limited to each timer process alone, it's easy to show
;; correct linear use of the various pointers.

(require racket/set)
(require data/heap)
(require syndicate/protocol/advertise)

(struct pending-timer (deadline label) #:transparent)
(struct timer-interrupt (counter now sealed-labels) #:transparent)

(provide (struct-out set-timer)
	 (struct-out timer-expired)
	 spawn-timer-driver)

(struct set-timer (label msecs kind) #:prefab)
(struct timer-expired (label msecs) #:prefab)

(define timer-interrupt-projection (inbound (?! (timer-interrupt ? ? ?))))

(struct timer-state (active-count interrupt-counter) #:transparent)

(define (spawn-timer-driver)
  (define control-ch (make-channel))
  (thread (lambda () (timer-driver-thread-main control-ch)))
  (define (timer-driver e state)
    (match-define (timer-state old-active-count old-interrupt-counter) state)
    (match e
      [(patch added _removed)
       ;; 21 Jan 2016.
       ;;
       ;; Previously, this driver caused the background thread to call
       ;; send-ground-message to indicate that a timer had expired.
       ;; However, this can lead to a race! In cases where a timer
       ;; expires very soon, the channel-put of the set-timer
       ;; instruction leads shortly thereafter to a
       ;; send-ground-message which then races the establishment of
       ;; the metalevel-1 subscription to the timer-expired events
       ;; that are coming from the background thread.
       ;;
       ;; The race cannot occur in the sequential implementation
       ;; because the dataspace makes sure to enqueue the transition
       ;; actions resulting from the set-timer message delivery ahead
       ;; of any enqueueing of the timer-expired ground message, so
       ;; that by the time the ground message is processed, the
       ;; relevant subscription always exists.
       ;;
       ;; In a looser implementation, however, this level of
       ;; synchronised activity may not exist, and the ground message
       ;; may overtake the subscription establishment.
       ;;
       ;; Therefore, I've changed the driver to instead use ground
       ;; /assertions/ to signal expired timers. Upon processing of
       ;; such an assertion, the driver cleans it up. This is very
       ;; similar to hardware interrupts, where the driver has to
       ;; "clear the interrupt" in order to let the system continue
       ;; properly.
       ;;
       ;; 3 Oct 2017.
       ;;
       ;; An additional kind of race can happen, even with ground
       ;; patches! If enough external ground-events spam the queue,
       ;; AND some other party is setting timers fast enough, AND some
       ;; listener for timer-expired at a particular label is
       ;; persistent, THEN this actor will observe a "stale"
       ;; interrupt, leading to duplicate timer-expired messages. This
       ;; happens because our clearing of the interrupt assertions is
       ;; delayed for long enough for the set-timer message to come in
       ;; and reestablish interest in interrupt assertions, before the
       ;; old ones have had a change to be retracted, so they appear
       ;; again.
       ;;
       ;; The fix relies on (a) no patch coalescing and (b) no
       ;; reordering of events. Each interrupt is given a number which
       ;; monotonically increases. We remember the most recent counter
       ;; we have processed, and ignore interrupts numbered at or
       ;; below it.
       ;;
       ;; This could be expressed using range matching if our tries
       ;; supported that: we'd express interest in ALL counter values,
       ;; retracting interest in them one at a time LOCALLY at the
       ;; same time we sent our (potentially heavily delayed) ground
       ;; patch on its way. We'd be relying on compression of adjacent
       ;; sequence numbers to keep the tries trim.
       ;;
       ;; Absent that, our hacky depend-on-no-coalescing-or-reordering
       ;; solution will do.
       ;;
       (define-values (active-count interrupt-counter actions interrupt-clearing-patch)
         (for/fold [(active-count old-active-count)
                    (interrupt-counter old-interrupt-counter)
                    (actions '())
                    (interrupt-clearing-patch patch-empty)]
                   [(interrupt (trie-project/set/single added timer-interrupt-projection))]
           (match-define (timer-interrupt candidate-interrupt-counter now sealed-labels) interrupt)
           (define labels (seal-contents sealed-labels))
           (if (> candidate-interrupt-counter old-interrupt-counter)
               (values (- active-count (length labels))
                       (max candidate-interrupt-counter interrupt-counter)
                       (cons actions (for/list [(label labels)]
                                       (message (timer-expired label now))))
                       (patch-seq interrupt-clearing-patch (retract interrupt)))
               (values active-count interrupt-counter actions interrupt-clearing-patch))))
       (send-ground-patch interrupt-clearing-patch)
       (transition (timer-state active-count interrupt-counter)
                   (cons actions (when (zero? active-count)
                                   (unsub (inbound (timer-interrupt ? ? ?))))))]
      [(message (and instruction (set-timer _ _ _)))
       (channel-put control-ch instruction)
       (transition (timer-state (+ old-active-count 1) old-interrupt-counter)
                   (when (= old-active-count 0) (sub (inbound (timer-interrupt ? ? ?)))))]
      [_ #f]))
  (actor #:name 'drivers/timer
         timer-driver
         (timer-state 0 -1)
         (patch-seq (sub (set-timer ? ? ?))
                    (pub (timer-expired ? ?)))))

(define (timer-driver-thread-main control-ch)
  (define heap (make-timer-heap))
  (let loop ((interrupt-counter 0))
    (sync (match (next-timer heap)
	    [#f never-evt]
	    [t (handle-evt (alarm-evt (pending-timer-deadline t))
			   (lambda (_dummy)
                             (define now (current-inexact-milliseconds))
                             (define labels (fire-timers! heap now))
                             (send-ground-patch
                              (assert (timer-interrupt interrupt-counter now (seal labels))))
			     (loop (+ interrupt-counter 1))))])
	  (handle-evt control-ch
		      (match-lambda
		       [(set-timer label msecs 'relative)
			(install-timer! heap label (+ (current-inexact-milliseconds) msecs))
			(loop interrupt-counter)]
		       [(set-timer label msecs 'absolute)
			(install-timer! heap label msecs)
			(loop interrupt-counter)]
		       ['quit (void)])))))

(define (make-timer-heap)
  (make-heap (lambda (t1 t2) (<= (pending-timer-deadline t1) (pending-timer-deadline t2)))))

(define (next-timer heap)
  (and (positive? (heap-count heap))
       (heap-min heap)))

(define (fire-timers! heap now)
  (if (zero? (heap-count heap))
      '()
      (let ((m (heap-min heap)))
	(if (<= (pending-timer-deadline m) now)
	    (begin (heap-remove-min! heap)
		   (cons (pending-timer-label m)
			 (fire-timers! heap now)))
	    '()))))

(define (install-timer! heap label deadline)
  (heap-add! heap (pending-timer deadline label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-timer-driver)
