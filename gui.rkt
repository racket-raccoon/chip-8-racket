#lang racket/gui

(require "emu.rkt")

(define scale-factor 10)

(define frame
  (new frame%
       [label "CHIP-8"]
       [width (* screen-width scale-factor)]
       [height (* screen-height scale-factor)]))

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

(define chip (make-chip-8))
(define rom (file->bytes "roms/keypad-test.ch8"))
(load-rom! chip rom)

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

(define ticks/second 60)
(define opcodes/second 600)
(define ms/tick (quotient 1000 ticks/second))

(define timer
  (new timer%
       [notify-callback
        (λ ()
          (define refresh-requested? #f)
          (parameterize ([shifts-use-vy? #f])
            (for ([i (in-range (quotient opcodes/second ticks/second))])
              (step chip (λ () (set! refresh-requested? #t)))))
          (when refresh-requested? (send canvas refresh-now))
          (tick! chip bell))]))

(send frame show #t)
(send canvas focus)
(send timer start ms/tick)
