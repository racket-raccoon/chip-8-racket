#lang racket/gui

(require "emu.rkt"
         "scheduler.rkt"
         "config.rkt")

(define scale-factor 10)

; The GUI timer only wakes the scheduler up; emulated time comes from
; the monotonic clock inside scheduler-run
(define scheduler-poll-ms 8)

; The running configuration; the settings dialog edits a draft copy and
; applies it atomically when the user confirms
(define settings (compat 1800 #f 'increment-x+1 #t #t #f #f #f))

(define chip (make-chip-8))

; The loaded ROM's lifecycle state; the bytes are cached at load time so
; reset is deterministic even if the file changes on disk
(define current-rom-path #f)
(define current-rom-bytes #f)
(define current-rom-note #f)
(define paused? #f)
; A machine fault cannot be resumed past: most opcode errors happen after
; the PC has advanced. Reset, restart, or a successful load clears the fault.
(define machine-failed? #f)

; A ROM's setup note is an adjacent .txt file with the same base name;
; it is shown to the user as plain UTF-8 text and never parsed or normalized
(define (read-rom-note path)
  (define note-path (path-replace-extension path ".txt"))
  (and (file-exists? note-path)
       (file->string note-path)))

(define chip-8-frame%
  (class frame%
    (define/augment (on-close)
      (send timer stop)
      (exit 0))
    (super-new)))

(define frame
  (new chip-8-frame%
       [label "CHIP-8"]
       [style '(no-resize-border)]
       [stretchable-width #f]
       [stretchable-height #f]))

(define menu-bar
  (new menu-bar% [parent frame]))

(define file-menu
  (new menu%
       [label "&File"]
       [parent menu-bar]))

; The CHIP-8 interpreter will accept input from a 16-key keypad,
; with each key corresponding to a single unique hexadecimal digit
;
;  1 2 3 4                 1 2 3 C
;  Q W E R                 4 5 6 D
;  A S D F                 7 8 9 E
;  Z X C V                 A 0 B F

(define key-code->chip-key
  (hash #\1 #x1 #\2 #x2 #\3 #x3 #\4 #xC
        #\Q #x4 #\W #x5 #\E #x6 #\R #xD
        #\A #x7 #\S #x8 #\D #x9 #\F #xE
        #\Z #xA #\X #x0 #\C #xB #\V #xF))

(define (handle-key-code key-code action!)
  (when (char? key-code)
    (define key (char-upcase key-code))
    (when (hash-has-key? key-code->chip-key key)
      (action! chip (hash-ref key-code->chip-key key)))))

(define chip-8-canvas%
  (class canvas%
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (if (eq? key-code 'release)
          (handle-key-code (send event get-key-release-code) chip-8-key-released!)
          (handle-key-code key-code chip-8-key-pressed!)))
    (super-new)))

(define canvas
  (new chip-8-canvas%
       [parent frame]
       [paint-callback
        (λ (canvas dc)
          (send dc set-background "blue")
          (send dc clear)
          (send dc set-brush "yellow" 'solid)
          (for* ([y (in-range screen-height)]
                 [x (in-range screen-width)]
                 #:unless (zero? (chip-8-pixel-ref chip x y)))
            (send dc draw-rectangle
                  (* x scale-factor) (* y scale-factor)
                  scale-factor scale-factor)))]))

(send canvas min-client-width (* screen-width scale-factor))
(send canvas min-client-height (* screen-height scale-factor))

(define scheduler (make-scheduler (compat-opcodes/second settings)))

; Report one failure and leave the window usable. Execution faults cannot be
; resumed past, while a failed ROM load leaves the previous machine recoverable.
(define (machine-error! message [execution-failure? #t])
  (when execution-failure? (set! machine-failed? #t))
  (set! paused? #t)
  (send timer stop)
  (update-controls!)
  (message-box "CHIP-8 Error" message frame '(ok caution)))

(define timer
  (new timer%
       [notify-callback
        (λ ()
          (define refresh-requested? #f)
          (with-handlers ([exn:fail?
                           (λ (e) (machine-error! (exn-message e)))])
            (with-compat settings
              (λ ()
                (scheduler-run scheduler
                               (current-inexact-monotonic-milliseconds)
                               (λ () (step chip (λ () (set! refresh-requested? #t))))
                               (λ () (tick! chip bell))))))
          (when refresh-requested? (send canvas refresh-now)))]))

(new menu-item%
     [label "&Load ROM..."]
     [parent file-menu]
     [callback
      (λ (_item _event)
        (define selected-path
          (get-file
           "Load CHIP-8 ROM"
           frame
           (build-path (current-directory) "roms")
           #f ; default filename
           "ch8" ; default extension
           empty
           '(("CHIP-8 ROMs" "*.ch8")
             ("All files" "*.*"))))

        (when selected-path
          (load-rom-with-errors! selected-path)))])

; The menu's error boundary, factored out so scripts can drive the
; exact production path
(define (load-rom-with-errors! path)
  (with-handlers ([exn:fail?
                   (λ (e) (machine-error! (exn-message e) #f))])
    (load-rom-from-path! path)))

; Read-only display of the loaded ROM's path and its setup note
(define rom-info-dialog
  (new dialog%
       [label "ROM Info"]
       [parent frame]
       [stretchable-width #f]
       [stretchable-height #f]))

(define rom-info-path-message
  (new message%
       [parent rom-info-dialog]
       [label ""]
       [auto-resize #t]))

(define rom-info-note-field
  (new text-field%
       [parent rom-info-dialog]
       [label #f]
       [style '(multiple)]
       [min-height 160]))

; The editor is locked so the note stays read-only on screen
(let ([editor (send rom-info-note-field get-editor)])
  (send editor lock #t))

(new button%
     [parent rom-info-dialog]
     [label "Close"]
     [callback (λ (_button _event) (send rom-info-dialog show #f))])

(define (show-rom-info!)
  (send rom-info-path-message set-label (path->string current-rom-path))
  (define editor (send rom-info-note-field get-editor))
  ; a locked editor refuses inserts, so unlock just for the refresh
  (send editor lock #f)
  (send editor erase)
  (send editor insert current-rom-note)
  (send editor lock #t)
  (send rom-info-dialog show #t))

(define rom-info-item
  (new menu-item%
       [label "ROM &Info..."]
       [parent file-menu]
       [callback (λ (_item _event) (show-rom-info!))]))

(define machine-menu
  (new menu% [label "&Machine"] [parent menu-bar]))

(define pause-item
  (new menu-item%
       [label "&Pause"]
       [parent machine-menu]
       [callback (λ (_item _event) (toggle-pause!))]))

(define reset-item
  (new menu-item%
       [label "Rese&t"]
       [parent machine-menu]
       [callback (λ (_item _event) (reset-rom!))]))

(define restart-item
  (new menu-item%
       [label "&Restart"]
       [parent machine-menu]
       [callback (λ (_item _event) (restart-rom!))]))

; Reload the cached bytes into a clean machine and re-anchor time; the
; paused state after this is the caller's decision
(define (reload-cached-rom!)
  (load-rom! chip current-rom-bytes)
  (scheduler-rebase! scheduler))

(define (load-rom-from-path! path)
  ; Stage all I/O and validate the ROM before replacing the cached lifecycle
  ; state. A failed load therefore leaves the previous ROM available to reset.
  (define rom-bytes (file->bytes path))
  (define rom-note (read-rom-note path))
  (load-rom! chip rom-bytes)
  (set! current-rom-path path)
  (set! current-rom-bytes rom-bytes)
  (set! current-rom-note rom-note)
  (set! paused? #f)
  (set! machine-failed? #f)
  (scheduler-rebase! scheduler)
  (send timer start scheduler-poll-ms)
  (update-controls!)
  (send canvas refresh-now)
  (send canvas focus))

(define (toggle-pause!)
  (unless machine-failed?
    (set! paused? (not paused?))
    (if paused?
        (send timer stop)
        ; paused wall time must not queue overdue emulated work
        (begin (scheduler-rebase! scheduler)
               (send timer start scheduler-poll-ms)))
    (update-controls!)))

(define (reset-rom!)
  ; a fresh machine left paused at its initial state
  (reload-cached-rom!)
  (set! paused? #t)
  (set! machine-failed? #f)
  (send timer stop)
  (update-controls!)
  (send canvas refresh-now))

(define (restart-rom!)
  (reload-cached-rom!)
  (set! paused? #f)
  (set! machine-failed? #f)
  (send timer start scheduler-poll-ms)
  (update-controls!)
  (send canvas refresh-now))

; Keep the lifecycle controls honest about what is possible right now
(define (update-controls!)
  (define has-rom? (and current-rom-bytes #t))
  (send pause-item set-label (if paused? "&Resume" "&Pause"))
  (send pause-item enable (and has-rom? (not machine-failed?)))
  (send reset-item enable has-rom?)
  (send restart-item enable has-rom?)
  (send rom-info-item enable (and current-rom-note #t)))

; FX55/FX65 memory behaviors, in settings-display order
(define load-store-modes
  '((increment-x+1 "I += X + 1 (original CHIP-8)")
    (increment-x "I += X (CHIP-48)")
    (unchanged "I unchanged (SUPER-CHIP 1.1)")))

(define settings-dialog
  (new dialog%
       [label "CHIP-8 Settings"]
       [parent frame]
       [stretchable-width #f]
       [stretchable-height #f]))

; The configuration being edited; only OK copies it into settings, so
; Cancel and half-finished edits never touch the running machine
(define draft settings)

(define settings-panel
  (new vertical-panel%
       [parent settings-dialog]
       [alignment '(left top)]
       [border 12]
       [spacing 6]))

; Selection indexes into this list, with 'custom first
(define preset-keys (cons 'custom (map car presets)))

(define preset-choice
  (new choice%
       [parent settings-panel]
       [label "Preset"]
       [choices (cons "Custom" (map cdr preset-names))]
       [selection 0]
       [callback
        (λ (choice _event)
          (define idx (send choice get-selection))
          (unless (zero? idx)
            (set! draft (cdr (list-ref presets (sub1 idx))))
            (update-dialog-widgets!)))]))

(define speed-field
  (new text-field%
       [parent settings-panel]
       [label "Opcodes per second"]
       [init-value (number->string (compat-opcodes/second draft))]
       [callback
        (λ (field _event)
          (define n (parse-speed (send field get-value)))
          ; invalid text is ignored until it parses or OK rejects it
          (when n
            (set! draft (struct-copy compat draft [opcodes/second n]))
            (send preset-choice set-selection 0)))]))

(define advanced-panel
  (new group-box-panel%
       [parent settings-dialog]
       [label "Advanced"]
       [alignment '(left top)]
       [border 6]
       [spacing 6]))

; Every advanced edit drops the displayed preset to Custom
(define (mark-custom!)
  (send preset-choice set-selection 0))

(define memory-choice
  (new choice%
       [parent advanced-panel]
       [label "FX55/FX65 memory mode"]
       [choices (map second load-store-modes)]
       [selection 0]
       [callback
        (λ (choice _event)
          (set! draft
                (struct-copy
                 compat draft
                 [load-store-i-mode
                  (list-ref (map first load-store-modes)
                            (send choice get-selection))]))
          (mark-custom!))]))

(define fx1e-box
  (new check-box%
       [parent advanced-panel]
       [label "FX1E sets VF on overflow"]
       [value (compat-fx1e-sets-vf? draft)]
       [callback
        (λ (box _event)
          (set! draft (struct-copy
                       compat draft
                       [fx1e-sets-vf? (send box get-value)]))
          (mark-custom!))]))

(define logic-box
  (new check-box%
       [parent advanced-panel]
       [label "Logic operations clear VF"]
       [value (compat-logic-ops-clear-vf? draft)]
       [callback
        (λ (box _event)
          (set! draft (struct-copy
                       compat draft
                       [logic-ops-clear-vf? (send box get-value)]))
          (mark-custom!))]))

(define shifts-box
  (new check-box%
       [parent advanced-panel]
       [label "Shift instructions use VY"]
       [value (compat-shifts-use-vy? draft)]
       [callback
        (λ (box _event)
          (set! draft (struct-copy
                       compat draft
                       [shifts-use-vy? (send box get-value)]))
          (mark-custom!))]))

(define jump-box
  (new check-box%
       [parent advanced-panel]
       [label "BNNN jump uses VX"]
       [value (compat-jump-uses-vx? draft)]
       [callback
        (λ (box _event)
          (set! draft (struct-copy
                       compat draft
                       [jump-uses-vx? (send box get-value)]))
          (mark-custom!))]))

(define vblank-box
  (new check-box%
       [parent advanced-panel]
       [label "Draws wait for vertical blank (one per frame)"]
       [value (compat-draw-waits-for-vblank? draft)]
       [callback
        (λ (box _event)
          (set! draft (struct-copy
                       compat draft
                       [draw-waits-for-vblank? (send box get-value)]))
          (mark-custom!))]))

(define wrap-box
  (new check-box%
       [parent advanced-panel]
       [label "Wrap sprites at screen edges"]
       [value (compat-sprites-wrap? draft)]
       [callback
        (λ (box _event)
          (set! draft (struct-copy
                       compat draft
                       [sprites-wrap? (send box get-value)]))
          (mark-custom!))]))

; Refresh every widget from the draft; programmatic setters do not fire
; widget callbacks, so this cannot recurse
(define (update-dialog-widgets!)
  (send preset-choice set-selection (index-of preset-keys (compat-preset draft)))
  (send speed-field set-value (number->string (compat-opcodes/second draft)))
  (send memory-choice set-selection
        (index-of (map first load-store-modes) (compat-load-store-i-mode draft)))
  (send fx1e-box set-value (compat-fx1e-sets-vf? draft))
  (send logic-box set-value (compat-logic-ops-clear-vf? draft))
  (send shifts-box set-value (compat-shifts-use-vy? draft))
  (send jump-box set-value (compat-jump-uses-vx? draft))
  (send vblank-box set-value (compat-draw-waits-for-vblank? draft))
  (send wrap-box set-value (compat-sprites-wrap? draft)))

(define (settings-ok!)
  (define n (parse-speed (send speed-field get-value)))
  (cond
    [n
     (define speed-changed? (not (= (compat-opcodes/second settings) n)))
     (set! settings (struct-copy compat draft [opcodes/second n]))
     ; the new CPU cadence starts now; the 60 Hz phase continues
     (when speed-changed? (scheduler-set-speed! scheduler n))
     (send settings-dialog show #f)]
    [else
     (message-box "CHIP-8 Settings"
                  "Opcodes per second must be a whole number from 60 to 10000."
                  settings-dialog
                  '(ok caution))]))

(define dialog-button-panel
  (new horizontal-panel%
       [parent settings-dialog]
       [alignment '(right center)]))

(new button%
     [parent dialog-button-panel]
     [label "OK"]
     [callback (λ (_button _event) (settings-ok!))])

(new button%
     [parent dialog-button-panel]
     [label "Cancel"]
     [callback (λ (_button _event) (send settings-dialog show #f))])

(new menu-item%
     [label "&Settings..."]
     [parent file-menu]
     [callback
      (λ (_item _event)
        (set! draft settings)
        (update-dialog-widgets!)
        (send settings-dialog show #t)
        (send canvas focus))])

(update-controls!)

(send frame show #t)
(send canvas focus)
