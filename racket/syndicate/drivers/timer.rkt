#lang syndicate
;; Timer driver.

;; Uses mutable state internally, but because the scope of the
;; mutation is limited to each timer process alone, it's easy to show
;; correct linear use of the various pointers.

(require racket/set)
(require data/heap)
(require syndicate/protocol/advertise)

(struct pending-timer (deadline label) #:transparent)

(provide (struct-out set-timer)
	 (struct-out timer-expired)
	 spawn-timer-driver)

(struct set-timer (label msecs kind) #:prefab)
(struct timer-expired (label msecs) #:prefab)

(define expiry-projection (inbound (?! (timer-expired ? ?))))

(define (spawn-timer-driver)
  (define control-ch (make-channel))
  (thread (lambda () (timer-driver-thread-main control-ch)))
  (define (timer-driver e count)
    (match e
      [(patch added _removed)
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
       (define-values (new-count actions-rev interrupt-clearing-patch)
         (for/fold [(count count)
                    (actions-rev '())
                    (interrupt-clearing-patch patch-empty)]
                   [(expiry (trie-project/set/single added expiry-projection))]
           (values (- count 1)
                   (cons (message expiry) actions-rev)
                   (patch-seq interrupt-clearing-patch
                              (retract expiry)))))
       (send-ground-patch interrupt-clearing-patch)
       (transition new-count
                   (cons (reverse actions-rev)
                         (when (zero? new-count) (unsub (inbound (timer-expired ? ?))))))]
      [(message (and instruction (set-timer _ _ _)))
       (channel-put control-ch instruction)
       (transition (+ count 1)
                   (when (= count 0) (sub (inbound (timer-expired ? ?)))))]
      [_ #f]))
  (spawn #:name 'drivers/timer
         timer-driver
         0 ;; initial count
         (patch-seq (sub (set-timer ? ? ?))
                    (pub (timer-expired ? ?)))))

(define (timer-driver-thread-main control-ch)
  (define heap (make-timer-heap))
  (let loop ()
    (sync (match (next-timer heap)
	    [#f never-evt]
	    [t (handle-evt (timer-evt (pending-timer-deadline t))
			   (lambda (now)
                             (send-ground-patch
                              (for/fold [(interrupt-asserting-patch patch-empty)]
                                        [(expiry (fire-timers! heap now))]
                                (patch-seq interrupt-asserting-patch (assert expiry))))
			     (loop)))])
	  (handle-evt control-ch
		      (match-lambda
		       [(set-timer label msecs 'relative)
			(install-timer! heap label (+ (current-inexact-milliseconds) msecs))
			(loop)]
		       [(set-timer label msecs 'absolute)
			(install-timer! heap label msecs)
			(loop)]
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
		   (cons (timer-expired (pending-timer-label m) now)
			 (fire-timers! heap now)))
	    '()))))

(define (install-timer! heap label deadline)
  (define now (current-inexact-milliseconds))
  (heap-add! heap (pending-timer deadline label)))

;; Racket's alarm-evt is almost the right design for timeouts: its
;; synchronisation value should be the (or some) value of the clock
;; after the asked-for time. That way it serves as timeout and
;; clock-reader in one.
(define (timer-evt msecs)
  (handle-evt (alarm-evt msecs)
	      (lambda (_) (current-inexact-milliseconds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-timer-driver)
