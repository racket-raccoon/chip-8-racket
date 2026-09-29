#lang racket

(require "emu.rkt")

(provide compat
         compat?
         compat-opcodes/second
         compat-fx1e-sets-vf?
         compat-load-store-i-mode
         compat-logic-ops-clear-vf?
         compat-shifts-use-vy?
         compat-jump-uses-vx?
         compat-sprites-wrap?
         compat-draw-waits-for-vblank?
         original-chip8
         modern-chip8
         superchip-11-quirks
         presets
         preset-names
         compat-preset
         valid-speed?
         parse-speed
         with-compat)

; One immutable bundle of every compatibility option plus the CPU rate,
; so a settings change applies to the machine atomically.
(struct compat (opcodes/second
                fx1e-sets-vf?
                load-store-i-mode
                logic-ops-clear-vf?
                shifts-use-vy?
                jump-uses-vx?
                sprites-wrap?
                draw-waits-for-vblank?)
  #:transparent)

; Defaults per the CHIP-8 database platform matrix, in opcodes per 60 Hz
; frame: 15 original, 12 modern, 30 SUPER-CHIP.
(define original-chip8 (compat 900 #f 'increment-x+1 #t #t #f #f #t))
(define modern-chip8 (compat 720 #f 'increment-x+1 #f #t #f #f #f))
; Only the base-CHIP-8 opcode quirks of SUPER-CHIP 1.1 are modeled; the
; extended opcodes and 128 x 64 display do not exist in this emulator.
(define superchip-11-quirks (compat 1800 #f 'unchanged #f #f #t #f #f))

(define presets
  (list (cons 'original original-chip8)
        (cons 'modern modern-chip8)
        (cons 'superchip-11 superchip-11-quirks)))

(define preset-names
  (list (cons 'original "Original CHIP-8")
        (cons 'modern "Modern CHIP-8")
        (cons 'superchip-11 "SUPER-CHIP 1.1 (quirks only)")))

; The preset whose every field matches config, or 'custom.
(define (compat-preset config)
  (or (for/first ([named (in-list presets)]
                  #:when (equal? (cdr named) config))
        (car named))
      'custom))

; A useful GUI range; speeds outside it are rejected, never clamped.
(define (valid-speed? n)
  (and (real? n) (integer? n) (<= 60 n 10000)))

; Settings fields may hold any text; a whole number in range becomes
; the speed and anything else is #f.
(define (parse-speed text)
  (define n (string->number (string-trim text)))
  (and n (valid-speed? n) n))

; Run thunk with every emulation parameter bound from config.
(define (with-compat config thunk)
  (parameterize ([fx1e-sets-vf? (compat-fx1e-sets-vf? config)]
                 [load-store-i-mode (compat-load-store-i-mode config)]
                 [logic-ops-clear-vf? (compat-logic-ops-clear-vf? config)]
                 [shifts-use-vy? (compat-shifts-use-vy? config)]
                 [jump-uses-vx? (compat-jump-uses-vx? config)]
                 [sprites-wrap? (compat-sprites-wrap? config)]
                 [draw-waits-for-vblank?
                  (compat-draw-waits-for-vblank? config)])
    (thunk)))

(module+ test
  (require rackunit)

  (test-case "presets hold the exact documented matrix values"
             (check-equal? original-chip8
                           (compat 900 #f 'increment-x+1 #t #t #f #f #t))
             (check-equal? modern-chip8
                           (compat 720 #f 'increment-x+1 #f #t #f #f #f))
             (check-equal? superchip-11-quirks
                           (compat 1800 #f 'unchanged #f #f #t #f #f)))

  (test-case "compat-preset matches whole presets and reports custom"
             (check-eq? (compat-preset original-chip8) 'original)
             (check-eq? (compat-preset modern-chip8) 'modern)
             (check-eq? (compat-preset superchip-11-quirks) 'superchip-11)
             (check-eq? (compat-preset (struct-copy compat modern-chip8
                                                    [logic-ops-clear-vf? #t]))
                        'custom)
             (check-eq? (compat-preset (struct-copy compat modern-chip8
                                                    [opcodes/second 733]))
                        'custom))

  (test-case "speed validation accepts whole numbers from 60 to 10000"
             (check-eq? (parse-speed "900") 900)
             (check-eq? (parse-speed " 720 ") 720)
             (check-eq? (parse-speed "60") 60)
             (check-eq? (parse-speed "10000") 10000)
             (check-false (parse-speed "abc"))
             (check-false (parse-speed ""))
             (check-false (parse-speed "-5"))
             (check-false (parse-speed "20"))
             (check-false (parse-speed "20000"))
             (check-false (parse-speed "720.5"))
             (check-eq? (valid-speed? 733) #t)
             (check-eq? (valid-speed? 59.9) #f))

  (test-case "with-compat binds the emulation parameters from a config"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xB1)) ; 8XY1 should clear VF
             (chip-8-reg-v-set! chip #xA #xA0)
             (chip-8-reg-v-set! chip #xB #x0F)
             (chip-8-reg-v-set! chip #xF #x7B)

             (with-compat modern-chip8 (lambda () (step chip)))

             (check-eq? (chip-8-reg-v-ref chip #xA) #xAF)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x7B))

  (test-case "a preset round-trips through its speed text"
             (define text (number->string (compat-opcodes/second original-chip8)))
             (check-equal? (struct-copy compat original-chip8
                                        [opcodes/second (parse-speed text)])
                           original-chip8)))
