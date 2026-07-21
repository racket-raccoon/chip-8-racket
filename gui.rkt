#lang racket/gui

(require "emu.rkt")

(define scale-factor 10)

(define frame
  (new frame%
       [label "CHIP-8"]
       [width (* screen-width scale-factor)]
       [height (* screen-height scale-factor)]))

(define chip (make-chip-8))
(define rom (file->bytes "roms/trip8.ch8"))
(load-rom! chip rom)

(define canvas
  (new canvas% [parent frame]
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
          (for ([i (in-range (quotient opcodes/second ticks/second))])
            (step chip (λ () (send canvas refresh))))
          (tick! chip bell))]))

(send frame show #t)

(send timer start ms/tick)
