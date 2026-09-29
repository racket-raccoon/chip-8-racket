#lang racket

(provide make-scheduler
         scheduler?
         scheduler-opcodes-per-second
         scheduler-run
         scheduler-rebase!
         scheduler-set-speed!)

; Emulated time is elapsed monotonic time, not counted GUI callbacks:
; timer% alarms are event-queue events that can arrive early, late, or
; in bursts, so the GUI timer only wakes the scheduler up to poll.

(struct scheduler (opcodes-per-second last-ms next-cpu-ms next-timer-ms)
  #:transparent #:mutable)

(define timer-period (/ 1000.0 60.0))
; A debugger stop, laptop suspend, or modal dialog can keep callbacks
; from running for seconds; at most this much emulated time is caught up
; per callback before the remaining backlog is discarded.
(define max-backlog-ms 250.0)
; Deadlines within a microsecond of now are still due; this absorbs
; floating-point accumulation jitter only.
(define due-epsilon-ms 0.001)

(define (cpu-period opcodes-per-second)
  (/ 1000.0 opcodes-per-second))

(define (make-scheduler [opcodes-per-second 900]
                        [now (current-inexact-monotonic-milliseconds)])
  (scheduler opcodes-per-second
             now
             (+ now (cpu-period opcodes-per-second))
             (+ now timer-period)))

; Run every CPU step and 60 Hz tick whose deadline has passed, earliest
; deadline first. now is the monotonic millisecond time the caller read
; once. A blocked CPU step still consumes its deadline so that a later
; resume cannot burst through banked work.
(define (scheduler-run s now cpu-step! timer-tick!)
  (define due-until (min now (+ (scheduler-last-ms s) max-backlog-ms)))
  (define due? (lambda (deadline) (<= deadline (+ due-until due-epsilon-ms))))
  (let loop ()
    (define next-cpu (scheduler-next-cpu-ms s))
    (define next-timer (scheduler-next-timer-ms s))
    (cond
      ; The earlier deadline runs first; a tie runs the CPU event.
      [(and (due? next-timer) (< next-timer next-cpu))
       (set-scheduler-next-timer-ms! s (+ next-timer timer-period))
       (timer-tick!)
       (loop)]
      [(due? next-cpu)
       (set-scheduler-next-cpu-ms!
        s (+ next-cpu (cpu-period (scheduler-opcodes-per-second s))))
       (cpu-step!)
       (loop)]
      [(due? next-timer)
       (set-scheduler-next-timer-ms! s (+ next-timer timer-period))
       (timer-tick!)
       (loop)]
      [else (void)]))
  ; An absence longer than the backlog cap leaves overdue deadlines;
  ; shift them forward whole, which keeps their phase and drops the
  ; time that was missed while the GUI could not run.
  (when (> now (+ (scheduler-last-ms s) max-backlog-ms))
    (define skipped (- now due-until))
    (set-scheduler-next-cpu-ms! s (+ (scheduler-next-cpu-ms s) skipped))
    (set-scheduler-next-timer-ms! s (+ (scheduler-next-timer-ms s) skipped)))
  (set-scheduler-last-ms! s now))

; Re-anchor all deadlines after a pause, ROM load, reset, or restart:
; time spent paused is not overdue emulated work.
(define (scheduler-rebase! s [now (current-inexact-monotonic-milliseconds)])
  (set-scheduler-last-ms! s now)
  (set-scheduler-next-cpu-ms!
   s (+ now (cpu-period (scheduler-opcodes-per-second s))))
  (set-scheduler-next-timer-ms! s (+ now timer-period)))

; Change the CPU rate going forward only; the 60 Hz timer phase and its
; next deadline are untouched.
(define (scheduler-set-speed! s opcodes-per-second
                              [now (current-inexact-monotonic-milliseconds)])
  (set-scheduler-opcodes-per-second! s opcodes-per-second)
  (set-scheduler-next-cpu-ms! s (+ now (cpu-period opcodes-per-second))))

(module+ test
  (require rackunit)

  (define (trace-scheduler ops)
    ; A scheduler plus counters for its two callbacks; the accessors
    ; are thunks because values would freeze the current numbers
    (define sched (make-scheduler ops 0.0))
    (define cpu-count 0)
    (define tick-count 0)
    (define events '())
    (define (run! now)
      (scheduler-run sched now
                     (lambda ()
                       (set! cpu-count (add1 cpu-count))
                       (set! events (cons 'cpu events)))
                     (lambda ()
                       (set! tick-count (add1 tick-count))
                       (set! events (cons 'tick events)))))
    (values sched run!
            (lambda () cpu-count)
            (lambda () tick-count)
            (lambda () (reverse events))))

  (test-case "a second of elapsed time produces exactly 60 timer ticks"
             (define-values (sched run! cpu-count tick-count events)
               (trace-scheduler 900))
             (for ([ms (in-range 10 1001 10)])
               (run! ms))
             (check-equal? (tick-count) 60)
             (check-equal? (cpu-count) 900))

  (test-case "CPU rates that do not divide 1000 keep their cadence"
             (for ([ops (in-list '(733 1800))])
               (define-values (sched run! cpu-count tick-count events)
                 (trace-scheduler ops))
               (for ([ms (in-range 10 1001 10)])
                 (run! ms))
               (check-equal? (cpu-count) ops)
               (check-equal? (tick-count) 60)))

  (test-case "early callbacks do no premature work"
             (define-values (sched run! cpu-count tick-count events)
               (trace-scheduler 900))
             (run! 0.5)
             (check-equal? (cpu-count) 0)
             (check-equal? (tick-count) 0)
             (run! 1.0)
             (check-equal? (cpu-count) 0)
             (check-equal? (tick-count) 0))

  (test-case "late callbacks catch up CPU and timers in deadline order"
             ; At 900 opcodes/second every 60 Hz frame holds exactly
             ; 15 CPU events, so the merged deadline order is fully
             ; determined.
             (define-values (sched run! cpu-count tick-count events)
               (trace-scheduler 900))
             (run! 40.0)
             (check-equal? (events)
                           (append (make-list 15 'cpu)
                                   (list 'tick)
                                   (make-list 15 'cpu)
                                   (list 'tick)
                                   (make-list 6 'cpu))))

  (test-case "waiting CPU steps consume deadlines without a resume burst"
             (define sched (make-scheduler 1800 0.0))
             (define calls 0)
             (define (blocked-step!) ; as if the CPU sat in FX0A
               (set! calls (add1 calls))
               'waiting-key)
             (for ([ms (in-range 10 1001 10)])
               (scheduler-run sched ms blocked-step! void))
             (check-equal? calls 1800)
             ; the ten milliseconds after release run at the usual rate
             (scheduler-run sched 1010.0 blocked-step! void)
             (check-equal? calls 1818))

  (test-case "rebase discards the time spent paused"
             (define-values (sched run! cpu-count tick-count events)
               (trace-scheduler 900))
             (run! 100.0)
             (check-equal? (tick-count) 6)
             (scheduler-rebase! sched 150.0)
             (for ([ms (in-range 160 201 10)])
               (run! ms))
             ; 50 ms of emulated time passed, not the 100 ms on the clock
             (check-equal? (tick-count) (+ 6 3)))

  (test-case "a speed change rebases CPU cadence but not the timer phase"
             (define-values (sched run! cpu-count tick-count events)
               (trace-scheduler 900))
             (run! 100.0)
             (check-equal? (cpu-count) 90)
             (check-equal? (tick-count) 6)
             (scheduler-set-speed! sched 1800 100.0)
             (for ([ms (in-range 110 201 10)])
               (run! ms))
             (check-equal? (cpu-count) (+ 90 180))
             ; the 60 Hz phase continued where it was
             (check-equal? (tick-count) 12))

  (test-case "a very long absence is capped and then rebased"
             (define-values (sched run! cpu-count tick-count events)
               (trace-scheduler 900))
             (run! 5000.0)
             ; at most 250 ms of backlog runs in one callback
             (check-equal? (cpu-count) 225)
             (check-equal? (tick-count) 15)
             ; the remainder was discarded, not queued for later
             (run! 5010.0)
             (check-equal? (cpu-count) (+ 225 9))
             (check-equal? (tick-count) 15)))
