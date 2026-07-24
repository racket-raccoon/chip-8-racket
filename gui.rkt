#lang racket/gui

(require "emu.rkt")

(define scale-factor 10)

(define ticks/second 60)
(define opcodes/second 1200)
(define ms/tick (quotient 1000 ticks/second))

(struct compat (fx1e-sets-vf?
                load-store-increments-i?
                logic-ops-clear-vf?
                shifts-use-vy?
                jump-uses-vx?)
  #:transparent #:mutable)

(define settings (compat #f #t #t #t #f))

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
  (new menu-bar%
       [parent frame]))

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
  (new chip-8-canvas% [parent frame]
       [paint-callback
        (λ (canvas dc)
          (send dc set-background "blue")
          (send dc clear)
          (send dc set-brush "yellow" 'solid)
          (for* ([y (in-range screen-height)]
                 [x (in-range screen-width)]
                 #:unless (zero? (chip-8-pixel-ref chip x y)))
            (send dc draw-rectangle (* x scale-factor) (* y scale-factor)
                  scale-factor scale-factor)))]))

(send canvas min-client-width (* screen-width scale-factor))
(send canvas min-client-height (* screen-height scale-factor))

(define timer
  (new timer%
       [notify-callback
        (λ ()
          (define refresh-requested? #f)
          (parameterize ([fx1e-sets-vf? (compat-fx1e-sets-vf? settings)]
                         [load-store-increments-i? (compat-load-store-increments-i? settings)]
                         [logic-ops-clear-vf? (compat-logic-ops-clear-vf? settings)]
                         [shifts-use-vy? (compat-shifts-use-vy? settings)]
                         [jump-uses-vx? (compat-jump-uses-vx? settings)])
            (for ([_ (in-range (quotient opcodes/second ticks/second))])
              (step chip (λ () (set! refresh-requested? #t)))))
          (when refresh-requested? (send canvas refresh-now))
          (tick! chip bell))]))

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
          (send canvas refresh-now)
          (send timer start ms/tick)))])

(define settings-dialog
  (new dialog%
       [label "CHIP-8 Settings"]
       [parent frame]
       [stretchable-width #f]
       [stretchable-height #f]))

(define settings-panel
  (new vertical-panel%
       [parent settings-dialog]
       [alignment '(left top)]
       [border 12]
       [spacing 6]))

(new check-box%
     [parent settings-panel]
     [label "FX1E sets VF on overflow"]
     [value (compat-fx1e-sets-vf? settings)]
     [callback
      (λ (box _event)
        (set-compat-fx1e-sets-vf?! settings
                                   (send box get-value)))])

(new check-box%
     [parent settings-panel]
     [label "FX55/FX65 increment I"]
     [value (compat-load-store-increments-i? settings)]
     [callback
      (λ (box _event)
        (set-compat-load-store-increments-i?! settings
                                              (send box get-value)))])

(new check-box%
     [parent settings-panel]
     [label "Logic operations clear VF"]
     [value (compat-logic-ops-clear-vf? settings)]
     [callback
      (λ (box _event)
        (set-compat-logic-ops-clear-vf?! settings
                                         (send box get-value)))])

(new check-box%
     [parent settings-panel]
     [label "Shift instructions use VY"]
     [value (compat-shifts-use-vy? settings)]
     [callback
      (λ (box _event)
        (set-compat-shifts-use-vy?! settings
                                    (send box get-value)))])

(new check-box%
     [parent settings-panel]
     [label "BNNN jump uses VX"]
     [value (compat-jump-uses-vx? settings)]
     [callback
      (λ (box _event)
        (set-compat-jump-uses-vx?! settings
                                   (send box get-value)))])

(new menu-item%
     [label "&Settings..."]
     [parent file-menu]
     [callback
      (λ (_item _event)
        (send settings-dialog show #t)
        (send canvas focus))])

(send frame show #t)
(send canvas focus)
