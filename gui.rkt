#lang racket/gui

(require "emu.rkt"
         "scheduler.rkt"
         "config.rkt")

(define scale-factor 10)

(define opcodes/second 1800)
; The GUI timer only wakes the scheduler up; emulated time comes from
; the monotonic clock inside scheduler-run
(define scheduler-poll-ms 8)

; The running configuration; the settings dialog edits a draft copy and
; applies it atomically when the user confirms
(define settings (compat 1800 #f 'increment-x+1 #t #t #f #f #f))

(define chip (make-chip-8))

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

(define scheduler (make-scheduler opcodes/second))

(define timer
  (new timer%
       [notify-callback
        (λ ()
          (define refresh-requested? #f)
          (with-compat settings
            (λ ()
              (scheduler-run scheduler
                             (current-inexact-monotonic-milliseconds)
                             (λ () (step chip (λ () (set! refresh-requested? #t))))
                             (λ () (tick! chip bell)))))
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
          (define rom (file->bytes selected-path))
          (load-rom! chip rom)
          (scheduler-rebase! scheduler)
          (send canvas refresh-now)
          (send timer start scheduler-poll-ms)))])

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

(send frame show #t)
(send canvas focus)
