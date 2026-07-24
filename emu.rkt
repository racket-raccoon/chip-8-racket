#lang racket

(provide make-chip-8
         load-rom!
         tick!
         step
         chip-8-pixel-ref
         chip-8-key-pressed!
         chip-8-key-released!
         screen-width
         screen-height
         fx1e-sets-vf?
         load-store-increments-i?
         logic-ops-clear-vf?
         shifts-use-vy?
         jump-uses-vx?)

(struct chip-8
  (memory pc i reg-v stack sp framebuffer delay-timer sound-timer keypad
          pending-keypress)
  #:transparent #:mutable)

(define memory-size #x1000)
(define stack-size 12)
(define load-address #x200)
(define font-address #x050)
(define font-size 5)
(define screen-height 32)
(define screen-width 64)

;; ambiguous instructions!
; The original COSMAC VIP allows I to exceed 0xFFF and leaves VF unchanged.
; Some later interpreters set VF on overflow: CHIP-8 interpreter for Amiga
; behaved this way. At least one known game, Spacefight 2091!, relies on it
(define fx1e-sets-vf? (make-parameter #f))
; The original COSMAC VIP increments I by X + 1 after FX55 and FX65.
; CHIP-48 and SUPER-CHIP leave I unchanged. Default to the original behavior.
(define load-store-increments-i? (make-parameter #t))
; The original COSMAC VIP overwrites VF after 8XY1, 8XY2, and 8XY3 with
; the 1802's DF flag, whose value is unrelated to these logical operations.
; Compatibility modes conventionally model this side effect by clearing VF;
; CHIP-48 and SUPER-CHIP instead leave VF unchanged.
(define logic-ops-clear-vf? (make-parameter #t))
; The original COSMAC VIP shifts VY and stores the result in VX for 8XY6 and
; 8XYE. CHIP-48 and SUPER-CHIP instead shift VX in place and ignore VY.
(define shifts-use-vy? (make-parameter #t))
; The original COSMAC VIP jumps to NNN plus V0 for BNNN. CHIP-48 and
; SUPER-CHIP instead use the register selected by X, treating it as BXNN.
(define jump-uses-vx? (make-parameter #f))

(define (make-chip-8)
  (chip-8
   ;; 4096 (0x1000) memory locations, all of which are 8 bits
   (make-bytes memory-size)
   ;; program counter points at the current instruction in memory
   load-address
   ;; index register I, used to address memory locations 0x000 through 0xFFF
   0
   ;; 16 8-bit general-purpose variable registers, called V0 through VF
   (make-bytes 16)
   ;; stack space must be sufficient for up to 12 successive subroutine calls
   (make-vector stack-size)
   ;; stack pointer, index of the next free slot
   0
   ;; monochrome screen of 64 × 32 pixels,
   ;; top-left corner of the screen is assigned coordinates of (0x00, 0x00)
   ;; and the bottom-right is assigned (0x3F, 0x1F).
   (make-bytes (* screen-height screen-width))
   ; Delay timer, both timers are one byte in size, and as long as their value
   ; is above 0, they should be decremented by one 60 times per second (ie. at 60 Hz)
   0
   ; The sound timer makes computer “beep” as long as it’s above 0
   0
   ; 16-button keypad, boolean vector representing if button is down
   (make-vector 16 #f)
   ; first key pressed transition since the last instruction
   #f
   ))

(define (chip-8-reg-v-ref c x) (bytes-ref (chip-8-reg-v c) x))
(define (chip-8-reg-v-set! c x val) (bytes-set! (chip-8-reg-v c) x val))

(define (chip-8-pixel-ref c x y)
  (bytes-ref (chip-8-framebuffer c)
             (+ x (* y screen-width))))
(define (chip-8-pixel-set! c x y val)
  (bytes-set! (chip-8-framebuffer c)
              (+ x (* y screen-width)) val))

(define (chip-8-stack-peek c)
  (define stack (chip-8-stack c))
  (define sp (chip-8-sp c))
  (if (zero? sp) #f (vector-ref stack (sub1 sp))))

(define (chip-8-stack-push! c val)
  (define stack (chip-8-stack c))
  (define sp (chip-8-sp c))
  (when (= sp stack-size) (error "Stack Overflow"))
  (vector-set! stack sp val)
  (set-chip-8-sp! c (add1 sp)))

(define (chip-8-stack-pop! c)
  (define sp (chip-8-sp c))
  (when (zero? sp) (error "Stack Underflow"))
  (define top (chip-8-stack-peek c))
  (vector-set! (chip-8-stack c) (sub1 sp) 0) ; optional
  (set-chip-8-sp! c (sub1 sp))
  top)

(define (chip-8-key-held? c n)
  (vector-ref (chip-8-keypad c) n))

(define (chip-8-key-pressed! c n)
  (define keypad (chip-8-keypad c))
  (unless (chip-8-pending-keypress c)
    (unless (vector-ref keypad n) ; #f -> #t transition
      (set-chip-8-pending-keypress! c n)))
  (vector-set! keypad n #t))

(define (chip-8-key-released! c n)
  (define keypad (chip-8-keypad c))
  (vector-set! keypad n #f))

(define default-font
  (bytes
   #xF0 #x90 #x90 #x90 #xF0 ; 0
   #x20 #x60 #x20 #x20 #x70 ; 1
   #xF0 #x10 #xF0 #x80 #xF0 ; 2
   #xF0 #x10 #xF0 #x10 #xF0 ; 3
   #x90 #x90 #xF0 #x10 #x10 ; 4
   #xF0 #x80 #xF0 #x10 #xF0 ; 5
   #xF0 #x80 #xF0 #x90 #xF0 ; 6
   #xF0 #x10 #x20 #x40 #x40 ; 7
   #xF0 #x90 #xF0 #x90 #xF0 ; 8
   #xF0 #x90 #xF0 #x10 #xF0 ; 9
   #xF0 #x90 #xF0 #x90 #x90 ; A
   #xE0 #x90 #xE0 #x90 #xE0 ; B
   #xF0 #x80 #x80 #x80 #xF0 ; C
   #xE0 #x90 #x90 #x90 #xE0 ; D
   #xF0 #x80 #xF0 #x80 #xF0 ; E
   #xF0 #x80 #xF0 #x80 #x80 ; F
   ))

(define (load-rom! c rom)
  ; reset emulator state
  (define mem (chip-8-memory c))
  (bytes-fill! mem 0)
  (bytes-copy! mem load-address rom)
  (bytes-copy! mem font-address default-font)
  (set-chip-8-pc! c load-address)
  (set-chip-8-i! c 0)
  (bytes-fill! (chip-8-reg-v c) 0)
  (vector-fill! (chip-8-stack c) 0)
  (set-chip-8-sp! c 0)
  (bytes-fill! (chip-8-framebuffer c) 0)
  (set-chip-8-delay-timer! c 0)
  (set-chip-8-sound-timer! c 0)
  (vector-fill! (chip-8-keypad c) #f)
  (set-chip-8-pending-keypress! c #f))

(define (fetch-opcode c)
  (define mem (chip-8-memory c))
  (define pc (chip-8-pc c))
  (define high (bytes-ref mem pc))
  (define low (bytes-ref mem (add1 pc)))
  (bitwise-ior (arithmetic-shift high 8) low))

(define (decompose-opcode opcode)
  ;; 0x [family] X Y N
  ;; 0x [family] X N N
  ;; 0x [family] N N N

  ; first nibble
  (define family (arithmetic-shift opcode (- 12)))
  ; second nibble
  (define X (bitwise-and (arithmetic-shift opcode (- 8)) #xF))
  ; third nibble
  (define Y (bitwise-and (arithmetic-shift opcode (- 4)) #xF))
  ; fourth nibble
  (define N (bitwise-and opcode #xF))
  ; second byte. 8-bit immediate number
  (define NN (bitwise-and opcode #xFF))
  ; second, third and fourth nibbles. 12-bit immediate memory address
  (define NNN (bitwise-and opcode #xFFF))
  (list family X Y N NN NNN))

(define (chip-8-draw-sprite! c X Y N)
  (define x (remainder (chip-8-reg-v-ref c X) screen-width)) ; column
  (define y (remainder (chip-8-reg-v-ref c Y) screen-height)) ; row
  (chip-8-reg-v-set! c #xF #x00)
  (for ([n (in-range N)] #:break (>= (+ y n) screen-height))
    (define sprite-row (bytes-ref (chip-8-memory c) (+ (chip-8-i c) n)))
    (for ([bit (in-inclusive-range 7 0 -1)]
          [j (in-naturals)]
          #:when (bitwise-bit-set? sprite-row bit)
          #:break (>= (+ x j) screen-width))
      (define old (chip-8-pixel-ref c (+ x j) (+ y n)))
      (unless (zero? old) (chip-8-reg-v-set! c #xF #x01))
      (chip-8-pixel-set! c (+ x j) (+ y n) (bitwise-xor old #x1)))))

(define (skip! c [n 1]) (set-chip-8-pc! c (+ (* 2 n) (chip-8-pc c))))

(define (tick! c [beep void])
  (define delay-timer (chip-8-delay-timer c))
  (unless (zero? delay-timer)
    (set-chip-8-delay-timer! c (sub1 delay-timer)))
  (define sound-timer (chip-8-sound-timer c))
  (when (positive? sound-timer) (beep))
  (unless (zero? sound-timer)
    (set-chip-8-sound-timer! c (sub1 sound-timer))))

(define (step c [request-redraw void])
  ;; FETCH
  (define opcode (fetch-opcode c))
  (skip! c)
  ;; DECODE & EXECUTE
  (match-define (list family X Y N NN NNN) (decompose-opcode opcode))
  ;; https://github.com/mattmikolay/chip-8/wiki/CHIP%E2%80%908-Instruction-Set
  (match (list family X Y N)
    ; 00E0 Clear the screen
    [(list #x0 #x0 #xE #x0) (bytes-fill! (chip-8-framebuffer c) 0) (request-redraw)]
    ; 00EE Return from a subroutine
    [(list #x0 #x0 #xE #xE) (set-chip-8-pc! c (chip-8-stack-pop! c))]
    ; 0NNN Execute machine language subroutine - unsupported by this emulator
    [(list #x0 _ _ _) (error "Subroutines written in machine language are unsupported")]
    ; 1NNN Jump to address NNN
    [(list #x1 _ _ _) (set-chip-8-pc! c NNN)]
    ; 2NNN Execute subroutine starting at address NNN
    [(list #x2 _ _ _) (chip-8-stack-push! c (chip-8-pc c)) (set-chip-8-pc! c NNN)]
    ; 3XNN Skip the following instruction if the value of register VX equals NN
    [(list #x3 _ _ _) (when (= (chip-8-reg-v-ref c X) NN) (skip! c))]
    ; 4XNN Skip the following instruction if the value of register VX is not equal to NN
    [(list #x4 _ _ _) (unless (= (chip-8-reg-v-ref c X) NN) (skip! c))]
    ; 5XY0 Skip the following instruction if the value of register VX is equal to the value of register VY
    [(list #x5 _ _ #x0) (when (= (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)) (skip! c))]
    ; 6XNN Store number NN in register VX
    [(list #x6 _ _ _) (chip-8-reg-v-set! c X NN)]
    ; 7XNN Add the value NN to register VX
    [(list #x7 _ _ _)
     (define old-val (chip-8-reg-v-ref c X))
     (define new-val (bitwise-and (+ old-val NN) #xFF))
     (chip-8-reg-v-set! c X new-val)]
    ; 8XY0 Store the value of register VY in register VX
    [(list #x8 _ _ #x0) (chip-8-reg-v-set! c X (chip-8-reg-v-ref c Y))]
    ; 8XY1 Set VX to VX OR VY
    [(list #x8 _ _ #x1)
     (chip-8-reg-v-set! c X (bitwise-ior (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)))
     (when (logic-ops-clear-vf?) (chip-8-reg-v-set! c #xF #x00))]
    ; 8XY2 Set VX to VX AND VY
    [(list #x8 _ _ #x2)
     (chip-8-reg-v-set! c X (bitwise-and (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)))
     (when (logic-ops-clear-vf?) (chip-8-reg-v-set! c #xF #x00))]
    ; 8XY3 Set VX to VX XOR VY
    [(list #x8 _ _ #x3)
     (chip-8-reg-v-set! c X (bitwise-xor (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)))
     (when (logic-ops-clear-vf?) (chip-8-reg-v-set! c #xF #x00))]
    ; 8XY4 Add the value of register VY to register VX
    ;      Set VF to 01 if a carry occurs
    ;      Set VF to 00 if a carry does not occur
    [(list #x8 _ _ #x4)
     (define sum (+ (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)))
     (chip-8-reg-v-set! c X (bitwise-and sum #xFF))
     (chip-8-reg-v-set! c #xF (if (> sum #xFF) 1 0))]
    ; 8XY5 Subtract the value of register VY from register VX
    ;      Set VF to 00 if a borrow occurs
    ;      Set VF to 01 if a borrow does not occur
    [(list #x8 _ _ #x5)
     (define diff (- (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)))
     (chip-8-reg-v-set! c X (bitwise-and diff #xFF))
     (chip-8-reg-v-set! c #xF (if (negative? diff) 0 1))]
    ; 8XY6 Store the value of register VY shifted right one bit in register VX.
    ;      Set register VF to the least significant bit prior to the shift;
    ;      VY is unchanged
    [(list #x8 _ _ #x6)
     (when (shifts-use-vy?) (chip-8-reg-v-set! c X (chip-8-reg-v-ref c Y)))
     (define vx (chip-8-reg-v-ref c X))
     (define new-vf (bitwise-and vx 1))
     (chip-8-reg-v-set! c X (arithmetic-shift vx -1))
     (chip-8-reg-v-set! c #xF new-vf)]
    ; 8XY7 Set register VX to the value of VY minus VX
    ;      Set VF to 00 if a borrow occurs
    ;      Set VF to 01 if a borrow does not occur
    [(list #x8 _ _ #x7)
     (define diff (- (chip-8-reg-v-ref c Y) (chip-8-reg-v-ref c X)))
     (chip-8-reg-v-set! c X (bitwise-and diff #xFF))
     (chip-8-reg-v-set! c #xF (if (negative? diff) 0 1))]
    ; 8XYE Shift VY left into VX, or shift VX in place in compatibility mode.
    ;      Set VF to the most significant bit prior to the shift;
    ;      VY is unchanged
    [(list #x8 _ _ #xE)
     (when (shifts-use-vy?) (chip-8-reg-v-set! c X (chip-8-reg-v-ref c Y)))
     (define shifted (arithmetic-shift (chip-8-reg-v-ref c X) 1))
     (chip-8-reg-v-set! c X (bitwise-and shifted #xFF))
     (chip-8-reg-v-set! c #xF (arithmetic-shift shifted -8))]
    ; 9XY0 Skip the following instruction if the value of register VX is not equal to the value of register VY
    [(list #x9 _ _ #x0) (unless (= (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)) (skip! c))]
    ; ANNN Store memory address NNN in register I
    [(list #xA _ _ _) (set-chip-8-i! c NNN)]
    ; BNNN Jump to address NNN + V0, or NNN + VX in compatibility mode
    [(list #xB _ _ _)
     (set-chip-8-pc! c (+ NNN (chip-8-reg-v-ref c (if (jump-uses-vx?) X #x0))))]
    ; CXNN Set VX to a random number with a mask of NN
    [(list #xC _ _ _) (chip-8-reg-v-set! c X (bitwise-and (random #x100) NN))]
    ; DXYN Draw a sprite at position VX, VY with N bytes of sprite data
    ;      starting at the address stored in I.  Set VF to 01 if any set
    ;      pixels are changed to unset, and 00 otherwise
    [(list #xD _ _ _) (chip-8-draw-sprite! c X Y N) (request-redraw)]
    ; EX9E Skip the following instruction if the key corresponding to the hex value
    ;      currently stored in register VX is pressed
    [(list #xE _ #x9 #xE)
     (when (chip-8-key-held? c (chip-8-reg-v-ref c X)) (skip! c))]
    ; EXA1 Skip the following instruction if the key corresponding to the hex value
    ;      currently stored in register VX is not pressed
    [(list #xE _ #xA #x1)
     (unless (chip-8-key-held? c (chip-8-reg-v-ref c X)) (skip! c))]
    ; FX07 Store the current value of the delay timer in register VX
    [(list #xF _ #x0 #x7) (chip-8-reg-v-set! c X (chip-8-delay-timer c))]
    ; FX0A Wait for a keypress and store the result in register VX
    [(list #xF _ #x0 #xA)
     (define key (chip-8-pending-keypress c))
     (if key (chip-8-reg-v-set! c X key) (skip! c -1))]
    ; FX15 Set the delay timer to the value of register VX
    [(list #xF _ #x1 #x5) (set-chip-8-delay-timer! c (chip-8-reg-v-ref c X))]
    ; FX18 Set the sound timer to the value of register VX
    [(list #xF _ #x1 #x8) (set-chip-8-sound-timer! c (chip-8-reg-v-ref c X))]
    ; FX1E Add the value stored in register VX to register I
    [(list #xF _ #x1 #xE)
     (define new-i (+ (chip-8-i c) (chip-8-reg-v-ref c X)))
     (set-chip-8-i! c new-i)
     (when (fx1e-sets-vf?) (chip-8-reg-v-set! c #xF (if (> new-i #xFFF) 1 0)))]
    ; FX29 Set I to the memory address of the sprite data corresponding to
    ;      the hexadecimal digit stored in register VX
    [(list #xF _ #x2 #x9)
     (set-chip-8-i! c
                    (+ font-address
                       (* font-size (chip-8-reg-v-ref c X))))]
    ; FX33 Store the binary-coded decimal equivalent of the value stored in
    ;      register VX at addresses I, I + 1, and I + 2
    [(list #xF _ #x3 #x3)
     (define mem (chip-8-memory c))
     (define i (chip-8-i c))
     (define vx (chip-8-reg-v-ref c X))
     (for ([j (in-naturals)][k (in-list '(2 1 0))])
       (define power (expt 10 j))
       (bytes-set! mem (+ i k) (remainder (quotient vx power) 10)))]
    ; FX55 Store registers V0 through VX inclusive in memory starting at I.
    ;      Depending on compatibility mode, increment I by X + 1 afterward
    [(list #xF _ #x5 #x5)
     (define mem (chip-8-memory c))
     (define i (chip-8-i c))
     (for ([j (in-inclusive-range 0 X)])
       (bytes-set! mem (+ i j) (chip-8-reg-v-ref c j)))
     (when (load-store-increments-i?) (set-chip-8-i! c (+ i X 1)))]
    ; FX65 Fill registers V0 to VX inclusive with the values stored in memory
    ;      starting at address I. Depending on compatibility mode, increment I
    ;      by X + 1 after the operation
    [(list #xF _ #x6 #x5)
     (define mem (chip-8-memory c))
     (define i (chip-8-i c))
     (for ([j (in-inclusive-range 0 X)])
       (chip-8-reg-v-set! c j (bytes-ref mem (+ i j))))
     (when (load-store-increments-i?) (set-chip-8-i! c (+ i X 1)))]
    [else (error (string-append "Unknown opcode 0x" (~r opcode #:base 16 #:min-width 4 #:pad-string "0")))])
  ;; FX0A consumes a pending keypress; every other instruction discards it.
  (set-chip-8-pending-keypress! c #f))

(define test-rom
  (bytes #x00 #xE0 ; clear screen
         #x60 #x00 ; V0 = 0
         #x61 #x00 ; V1 = 0
         #xA2 #x0C ; I = 0x20C
         #xD0 #x15 ; draw 5-byte sprite at (V0, V1)
         #x12 #x0A ; jump to 0x20A forever

         #xF0 ; ####
         #x90 ; #..#
         #x90 ; #..#
         #x90 ; #..#
         #xF0 ; ####
         ))

(define minimal-rom (bytes #x12 #x00))

(module+ main
  (define chip (make-chip-8))
  (define rom (file->bytes "roms/test-opcode.ch8"))
  (load-rom! chip rom)
  (for ([_ (in-range 10000)])
    (step chip)))

(module+ test
  (require rackunit)

  (test-case "constructor"
             (define chip (make-chip-8))
             (check-equal? (chip-8-memory chip) (make-bytes memory-size 0)))

  (test-case "empty stack cannot be popped"
             (define chip (make-chip-8))

             (check-false (chip-8-stack-peek chip))
             (check-exn #rx"Stack Underflow"
                        (lambda () (chip-8-stack-pop! chip))))

  (test-case "stack stores addresses in LIFO order up to its capacity"
             (define chip (make-chip-8))

             (for ([address (in-range stack-size)])
               (chip-8-stack-push! chip address))

             (check-eq? (chip-8-sp chip) stack-size)
             (check-eq? (chip-8-stack-peek chip) (sub1 stack-size))
             (check-eq? (chip-8-sp chip) stack-size)
             (check-exn #rx"Stack Overflow"
                        (lambda () (chip-8-stack-push! chip #xABC)))

             (for ([address (in-range (sub1 stack-size) -1 -1)])
               (check-eq? (chip-8-stack-pop! chip) address))

             (check-eq? (chip-8-sp chip) 0)
             (check-false (chip-8-stack-peek chip)))

  (define (check-rom-loaded c rom)
    (check-eq? (chip-8-pc c) load-address)
    (define mem (chip-8-memory c))
    (define rom-len (bytes-length rom))
    (define interpreter-memory (make-bytes load-address))
    (bytes-copy! interpreter-memory font-address default-font)
    (check-equal? (subbytes mem 0 load-address) interpreter-memory)
    (check-equal? (subbytes mem load-address (+ load-address rom-len)) rom)
    (check-equal? (subbytes mem (+ load-address rom-len)) (make-bytes (- memory-size load-address rom-len))))

  (test-case "load-rom"
             (define chip (make-chip-8))
             (load-rom! chip test-rom)
             (check-rom-loaded chip test-rom)
             (load-rom! chip minimal-rom)
             (check-rom-loaded chip minimal-rom))

  (test-case "fetch-opcode"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xAB #xCD))
             (check-eq? (fetch-opcode chip) #xABCD))

  (test-case "decompose-opcode"
             (match-define (list family X Y N NN NNN) (decompose-opcode #xABCD))
             (check-eq? family #xA)
             (check-eq? X #xB)
             (check-eq? Y #xC)
             (check-eq? N #xD)
             (check-eq? NN #xCD)
             (check-eq? NNN #xBCD))

  (test-case "00E0 clears the screen and requests a redraw"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x00 #xE0))
             (bytes-fill! (chip-8-framebuffer chip) 1)
             (define redraw-count 0)

             (step chip (lambda () (set! redraw-count (add1 redraw-count))))

             (check-equal? (chip-8-framebuffer chip)
                           (make-bytes (* screen-height screen-width)))
             (check-eq? (chip-8-pc chip) (+ load-address 2))
             (check-eq? redraw-count 1))

  (test-case "1NNN jumps to address NNN"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x1A #xBC))

             (step chip)

             (check-eq? (chip-8-pc chip) #xABC))

  (test-case "2NNN calls a subroutine and 00EE returns"
             (define chip (make-chip-8))
             (load-rom! chip
                        (bytes #x22 #x08 ; call subroutine at 0x208
                               #x60 #xAA ; continuation: V0 = 0xAA
                               #x00 #x00 ; unused padding
                               #x00 #x00
                               #x60 #xBB ; subroutine: V0 = 0xBB
                               #x00 #xEE ; return
                               ))

             (step chip)
             (check-eq? (chip-8-pc chip) #x208)
             (check-eq? (chip-8-stack-peek chip) #x202)

             (step chip)
             (check-eq? (chip-8-reg-v-ref chip #x0) #xBB)
             (step chip)
             (check-eq? (chip-8-pc chip) #x202)
             (check-eq? (chip-8-sp chip) 0)

             (step chip)
             (check-eq? (chip-8-reg-v-ref chip #x0) #xAA))

  (test-case "3XNN skips only when VX equals NN"
             (define equal-chip (make-chip-8))
             (load-rom! equal-chip (bytes #x3A #x42))
             (chip-8-reg-v-set! equal-chip #xA #x42)
             (step equal-chip)
             (check-eq? (chip-8-pc equal-chip) (+ load-address 4))

             (define unequal-chip (make-chip-8))
             (load-rom! unequal-chip (bytes #x3A #x42))
             (chip-8-reg-v-set! unequal-chip #xA #x41)
             (step unequal-chip)
             (check-eq? (chip-8-pc unequal-chip) (+ load-address 2)))

  (test-case "4XNN skips only when VX does not equal NN"
             (define equal-chip (make-chip-8))
             (load-rom! equal-chip (bytes #x4A #x42))
             (chip-8-reg-v-set! equal-chip #xA #x42)
             (step equal-chip)
             (check-eq? (chip-8-pc equal-chip) (+ load-address 2))

             (define unequal-chip (make-chip-8))
             (load-rom! unequal-chip (bytes #x4A #x42))
             (chip-8-reg-v-set! unequal-chip #xA #x41)
             (step unequal-chip)
             (check-eq? (chip-8-pc unequal-chip) (+ load-address 4)))

  (test-case "5XY0 skips only when VX equals VY"
             (define equal-chip (make-chip-8))
             (load-rom! equal-chip (bytes #x5A #xB0))
             (chip-8-reg-v-set! equal-chip #xA #x42)
             (chip-8-reg-v-set! equal-chip #xB #x42)
             (step equal-chip)
             (check-eq? (chip-8-pc equal-chip) (+ load-address 4))

             (define unequal-chip (make-chip-8))
             (load-rom! unequal-chip (bytes #x5A #xB0))
             (chip-8-reg-v-set! unequal-chip #xA #x42)
             (chip-8-reg-v-set! unequal-chip #xB #x41)
             (step unequal-chip)
             (check-eq? (chip-8-pc unequal-chip) (+ load-address 2))

             (define invalid-chip (make-chip-8))
             (load-rom! invalid-chip (bytes #x5A #xB1))
             (check-exn #rx"Unknown opcode"
                        (lambda () (step invalid-chip))))

  (test-case "6XNN stores NN in register VX"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x6A #xCD))

             (step chip)

             (check-equal? (chip-8-reg-v chip)
                           (bytes #x00 #x00 #x00 #x00
                                  #x00 #x00 #x00 #x00
                                  #x00 #x00 #xCD #x00
                                  #x00 #x00 #x00 #x00)))

  (test-case "7XNN adds NN to VX and wraps at 8 bits"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x6F #xFE
                                    #x7F #x05))

             (step chip)
             (step chip)

             (check-eq? (chip-8-reg-v-ref chip #xF) #x03))

  (test-case "8XY0 copies VY into VX"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xB0))
             (chip-8-reg-v-set! chip #xA #x12)
             (chip-8-reg-v-set! chip #xB #xCD)

             (step chip)

             (check-eq? (chip-8-reg-v-ref chip #xA) #xCD)
             (check-eq? (chip-8-reg-v-ref chip #xB) #xCD))

  (test-case "8XY0 treats VF as an ordinary register"
             (define to-vf-chip (make-chip-8))
             (load-rom! to-vf-chip (bytes #x8F #xA0))
             (chip-8-reg-v-set! to-vf-chip #xA #x42)
             (step to-vf-chip)
             (check-eq? (chip-8-reg-v-ref to-vf-chip #xF) #x42)

             (define from-vf-chip (make-chip-8))
             (load-rom! from-vf-chip (bytes #x8A #xF0))
             (chip-8-reg-v-set! from-vf-chip #xF #x42)
             (step from-vf-chip)
             (check-eq? (chip-8-reg-v-ref from-vf-chip #xA) #x42)
             (check-eq? (chip-8-reg-v-ref from-vf-chip #xF) #x42))

  (test-case "8XY1, 8XY2, and 8XY3 perform logical operations and clear VF"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xB1
                                    #x8C #xD2
                                    #x8E #x93))
             (chip-8-reg-v-set! chip #xA #xA0)
             (chip-8-reg-v-set! chip #xB #x0F)
             (chip-8-reg-v-set! chip #xC #xF3)
             (chip-8-reg-v-set! chip #xD #x3C)
             (chip-8-reg-v-set! chip #xE #xAA)
             (chip-8-reg-v-set! chip #x9 #x0F)

             (chip-8-reg-v-set! chip #xF #x01)
             (step chip)
             (check-eq? (chip-8-reg-v-ref chip #xA) #xAF)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x00)

             (chip-8-reg-v-set! chip #xF #x01)
             (step chip)
             (check-eq? (chip-8-reg-v-ref chip #xC) #x30)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x00)

             (chip-8-reg-v-set! chip #xF #x01)
             (step chip)
             (check-eq? (chip-8-reg-v-ref chip #xE) #xA5)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x00))

  (test-case "Logical operations can leave VF unchanged"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xB1
                                    #x8C #xD2
                                    #x8E #x93))
             (chip-8-reg-v-set! chip #xA #xA0)
             (chip-8-reg-v-set! chip #xB #x0F)
             (chip-8-reg-v-set! chip #xC #xF3)
             (chip-8-reg-v-set! chip #xD #x3C)
             (chip-8-reg-v-set! chip #xE #xAA)
             (chip-8-reg-v-set! chip #x9 #x0F)
             (chip-8-reg-v-set! chip #xF #x7B)

             (parameterize ([logic-ops-clear-vf? #f])
               (step chip)
               (step chip)
               (step chip))

             (check-eq? (chip-8-reg-v-ref chip #xA) #xAF)
             (check-eq? (chip-8-reg-v-ref chip #xC) #x30)
             (check-eq? (chip-8-reg-v-ref chip #xE) #xA5)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x7B))

  (test-case "8XY4 adds VY to VX and reports carry in VF"
             (define no-carry-chip (make-chip-8))
             (load-rom! no-carry-chip (bytes #x8A #xB4))
             (chip-8-reg-v-set! no-carry-chip #xA #x12)
             (chip-8-reg-v-set! no-carry-chip #xB #x34)
             (chip-8-reg-v-set! no-carry-chip #xF #x01)
             (step no-carry-chip)
             (check-eq? (chip-8-reg-v-ref no-carry-chip #xA) #x46)
             (check-eq? (chip-8-reg-v-ref no-carry-chip #xF) #x00)

             (define carry-chip (make-chip-8))
             (load-rom! carry-chip (bytes #x8A #xB4))
             (chip-8-reg-v-set! carry-chip #xA #xFE)
             (chip-8-reg-v-set! carry-chip #xB #x05)
             (step carry-chip)
             (check-eq? (chip-8-reg-v-ref carry-chip #xA) #x03)
             (check-eq? (chip-8-reg-v-ref carry-chip #xF) #x01))

  (test-case "8XY4 handles VF as an operand"
             (define x-is-vf-chip (make-chip-8))
             (load-rom! x-is-vf-chip (bytes #x8F #xA4))
             (chip-8-reg-v-set! x-is-vf-chip #xF #x02)
             (chip-8-reg-v-set! x-is-vf-chip #xA #x03)
             (step x-is-vf-chip)
             ;; The flag written last wins over the sum stored in VX.
             (check-eq? (chip-8-reg-v-ref x-is-vf-chip #xF) #x00)

             (define y-is-vf-chip (make-chip-8))
             (load-rom! y-is-vf-chip (bytes #x8A #xF4))
             (chip-8-reg-v-set! y-is-vf-chip #xA #xFE)
             (chip-8-reg-v-set! y-is-vf-chip #xF #x02)
             (step y-is-vf-chip)
             ;; The old value of VF participates in the addition.
             (check-eq? (chip-8-reg-v-ref y-is-vf-chip #xA) #x00)
             (check-eq? (chip-8-reg-v-ref y-is-vf-chip #xF) #x01))

  (test-case "8XY5 subtracts VY from VX and reports borrow in VF"
             (define no-borrow-chip (make-chip-8))
             (load-rom! no-borrow-chip (bytes #x8A #xB5))
             (chip-8-reg-v-set! no-borrow-chip #xA #x34)
             (chip-8-reg-v-set! no-borrow-chip #xB #x12)
             (step no-borrow-chip)
             (check-eq? (chip-8-reg-v-ref no-borrow-chip #xA) #x22)
             (check-eq? (chip-8-reg-v-ref no-borrow-chip #xF) #x01)

             (define borrow-chip (make-chip-8))
             (load-rom! borrow-chip (bytes #x8A #xB5))
             (chip-8-reg-v-set! borrow-chip #xA #x03)
             (chip-8-reg-v-set! borrow-chip #xB #x05)
             (step borrow-chip)
             (check-eq? (chip-8-reg-v-ref borrow-chip #xA) #xFE)
             (check-eq? (chip-8-reg-v-ref borrow-chip #xF) #x00)

             (define equal-chip (make-chip-8))
             (load-rom! equal-chip (bytes #x8A #xB5))
             (chip-8-reg-v-set! equal-chip #xA #x42)
             (chip-8-reg-v-set! equal-chip #xB #x42)
             (step equal-chip)
             (check-eq? (chip-8-reg-v-ref equal-chip #xA) #x00)
             (check-eq? (chip-8-reg-v-ref equal-chip #xF) #x01))

  (test-case "8XY5 handles VF as an operand"
             (define x-is-vf-chip (make-chip-8))
             (load-rom! x-is-vf-chip (bytes #x8F #xA5))
             (chip-8-reg-v-set! x-is-vf-chip #xF #x02)
             (chip-8-reg-v-set! x-is-vf-chip #xA #x03)
             (step x-is-vf-chip)
             ;; The flag written last wins over the difference stored in VX.
             (check-eq? (chip-8-reg-v-ref x-is-vf-chip #xF) #x00)

             (define y-is-vf-chip (make-chip-8))
             (load-rom! y-is-vf-chip (bytes #x8A #xF5))
             (chip-8-reg-v-set! y-is-vf-chip #xA #x05)
             (chip-8-reg-v-set! y-is-vf-chip #xF #x03)
             (step y-is-vf-chip)
             ;; The old value of VF participates in the subtraction.
             (check-eq? (chip-8-reg-v-ref y-is-vf-chip #xA) #x02)
             (check-eq? (chip-8-reg-v-ref y-is-vf-chip #xF) #x01))

  (test-case "8XY7 subtracts VX from VY and reports borrow in VF"
             (define no-borrow-chip (make-chip-8))
             (load-rom! no-borrow-chip (bytes #x8A #xB7))
             (chip-8-reg-v-set! no-borrow-chip #xA #x12)
             (chip-8-reg-v-set! no-borrow-chip #xB #x34)
             (step no-borrow-chip)
             (check-eq? (chip-8-reg-v-ref no-borrow-chip #xA) #x22)
             (check-eq? (chip-8-reg-v-ref no-borrow-chip #xF) #x01)

             (define borrow-chip (make-chip-8))
             (load-rom! borrow-chip (bytes #x8A #xB7))
             (chip-8-reg-v-set! borrow-chip #xA #x05)
             (chip-8-reg-v-set! borrow-chip #xB #x03)
             (step borrow-chip)
             (check-eq? (chip-8-reg-v-ref borrow-chip #xA) #xFE)
             (check-eq? (chip-8-reg-v-ref borrow-chip #xF) #x00)

             (define equal-chip (make-chip-8))
             (load-rom! equal-chip (bytes #x8A #xB7))
             (chip-8-reg-v-set! equal-chip #xA #x42)
             (chip-8-reg-v-set! equal-chip #xB #x42)
             (step equal-chip)
             (check-eq? (chip-8-reg-v-ref equal-chip #xA) #x00)
             (check-eq? (chip-8-reg-v-ref equal-chip #xF) #x01))

  (test-case "8XY6 shifts VY into VX in original mode"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xB6))
             (chip-8-reg-v-set! chip #xA #x82)
             (chip-8-reg-v-set! chip #xB #x03)

             (step chip)

             (check-eq? (chip-8-reg-v-ref chip #xA) #x01)
             (check-eq? (chip-8-reg-v-ref chip #xB) #x03)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x01))

  (test-case "8XY6 can shift VX in place and ignore VY"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xB6))
             (chip-8-reg-v-set! chip #xA #x82)
             (chip-8-reg-v-set! chip #xB #x03)
             (chip-8-reg-v-set! chip #xF #x01)

             (parameterize ([shifts-use-vy? #f])
               (step chip))

             (check-eq? (chip-8-reg-v-ref chip #xA) #x41)
             (check-eq? (chip-8-reg-v-ref chip #xB) #x03)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x00))

  (test-case "8XYE shifts VY into VX in original mode"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xBE))
             (chip-8-reg-v-set! chip #xA #x41)
             (chip-8-reg-v-set! chip #xB #x81)

             (step chip)

             (check-eq? (chip-8-reg-v-ref chip #xA) #x02)
             (check-eq? (chip-8-reg-v-ref chip #xB) #x81)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x01))

  (test-case "8XYE can shift VX in place and ignore VY"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x8A #xBE))
             (chip-8-reg-v-set! chip #xA #x41)
             (chip-8-reg-v-set! chip #xB #x80)
             (chip-8-reg-v-set! chip #xF #x01)

             (parameterize ([shifts-use-vy? #f])
               (step chip))

             (check-eq? (chip-8-reg-v-ref chip #xA) #x82)
             (check-eq? (chip-8-reg-v-ref chip #xB) #x80)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x00))

  (test-case "9XY0 skips only when VX does not equal VY"
             (define equal-chip (make-chip-8))
             (load-rom! equal-chip (bytes #x9A #xB0))
             (chip-8-reg-v-set! equal-chip #xA #x42)
             (chip-8-reg-v-set! equal-chip #xB #x42)
             (step equal-chip)
             (check-eq? (chip-8-pc equal-chip) (+ load-address 2))

             (define unequal-chip (make-chip-8))
             (load-rom! unequal-chip (bytes #x9A #xB0))
             (chip-8-reg-v-set! unequal-chip #xA #x42)
             (chip-8-reg-v-set! unequal-chip #xB #x41)
             (step unequal-chip)
             (check-eq? (chip-8-pc unequal-chip) (+ load-address 4))

             (define invalid-chip (make-chip-8))
             (load-rom! invalid-chip (bytes #x9A #xB1))
             (check-exn #rx"Unknown opcode"
                        (lambda () (step invalid-chip))))

  (test-case "ANNN stores NNN in register I"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xAB #xCD))

             (step chip)

             (check-eq? (chip-8-i chip) #xBCD))

  (test-case "BNNN jumps to NNN plus V0 in original mode"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xB2 #x20))
             (chip-8-reg-v-set! chip #x0 #x04)
             (chip-8-reg-v-set! chip #x2 #x10)

             (step chip)

             (check-eq? (chip-8-pc chip) #x224))

  (test-case "BNNN can jump to NNN plus VX in compatibility mode"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xB2 #x20))
             (chip-8-reg-v-set! chip #x0 #x04)
             (chip-8-reg-v-set! chip #x2 #x10)

             (parameterize ([jump-uses-vx? #t])
               (step chip))

             (check-eq? (chip-8-pc chip) #x230))

  (test-case "CXNN stores a masked random byte in VX"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xCA #xA5))

             (parameterize ([current-pseudo-random-generator
                             (make-pseudo-random-generator)])
               (random-seed 12345)
               (step chip))

             ;; The seeded random byte is 0x0D; 0x0D AND 0xA5 is 0x05.
             (check-eq? (chip-8-reg-v-ref chip #xA) #x05))

  (test-case "DXYN draws an in-bounds sprite at VX, VY"
             (define chip (make-chip-8))
             (load-rom! chip
                        (bytes #xDA #xB5
                               #xF0
                               #x90
                               #x90
                               #x90
                               #xF0))
             (set-chip-8-i! chip (+ load-address 2))
             (chip-8-reg-v-set! chip #xA 5)
             (chip-8-reg-v-set! chip #xB 7)

             (step chip)

             (define expected (make-bytes (* screen-height screen-width)))
             (for ([point '((5  7) (6  7) (7  7) (8  7)
                                   (5  8)               (8  8)
                                   (5  9)               (8  9)
                                   (5 10)               (8 10)
                                   (5 11) (6 11) (7 11) (8 11))])
               (match-define (list x y) point)
               (bytes-set! expected (+ x (* y screen-width)) 1))
             (check-equal? (chip-8-framebuffer chip) expected))

  (test-case "DXYN sets VF when two sprites overlap at one corner"
             (define chip (make-chip-8))
             (load-rom! chip
                        (bytes #xD0 #x12
                               #xD0 #x12
                               #xC0
                               #xC0))
             (set-chip-8-i! chip (+ load-address 4))

             (step chip)
             (check-eq? (chip-8-reg-v-ref chip #xF) 0)

             (chip-8-reg-v-set! chip #x0 1)
             (chip-8-reg-v-set! chip #x1 1)
             (step chip)

             (define expected (make-bytes (* screen-height screen-width)))
             (for ([point '((0 0) (1 0)
                                  (0 1)       (2 1)
                                  (1 2) (2 2))])
               (match-define (list x y) point)
               (bytes-set! expected (+ x (* y screen-width)) 1))
             (check-equal? (chip-8-framebuffer chip) expected)
             (check-eq? (chip-8-reg-v-ref chip #xF) 1))

  (test-case "DXYN normalizes its origin and clips at the screen edges"
             (define chip (make-chip-8))
             (load-rom! chip
                        (bytes #xD0 #x12
                               #xC0
                               #xC0))
             (set-chip-8-i! chip (+ load-address 2))
             (chip-8-reg-v-set! chip #x0 #xFF)
             (chip-8-reg-v-set! chip #x1 #xFF)

             (step chip)

             (define expected (make-bytes (* screen-height screen-width)))
             (bytes-set! expected
                         (+ (sub1 screen-width)
                            (* (sub1 screen-height) screen-width))
                         1)
             (check-equal? (chip-8-framebuffer chip) expected))

  (test-case "EX9E skips only when the key in VX is held"
             (define held-chip (make-chip-8))
             (load-rom! held-chip (bytes #xEA #x9E))
             (chip-8-reg-v-set! held-chip #xA #x5)
             (chip-8-key-pressed! held-chip #x5)
             (step held-chip)
             (check-eq? (chip-8-pc held-chip) (+ load-address 4))

             (define released-chip (make-chip-8))
             (load-rom! released-chip (bytes #xEA #x9E))
             (chip-8-reg-v-set! released-chip #xA #x5)
             (step released-chip)
             (check-eq? (chip-8-pc released-chip) (+ load-address 2)))

  (test-case "EXA1 skips only when the key in VX is not held"
             (define held-chip (make-chip-8))
             (load-rom! held-chip (bytes #xEA #xA1))
             (chip-8-reg-v-set! held-chip #xA #x5)
             (chip-8-key-pressed! held-chip #x5)
             (step held-chip)
             (check-eq? (chip-8-pc held-chip) (+ load-address 2))

             (define released-chip (make-chip-8))
             (load-rom! released-chip (bytes #xEA #xA1))
             (chip-8-reg-v-set! released-chip #xA #x5)
             (step released-chip)
             (check-eq? (chip-8-pc released-chip) (+ load-address 4)))

  (test-case "FX0A waits at the same instruction when no key was pressed"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x0A))
             (chip-8-reg-v-set! chip #xA #x42)

             (step chip)

             (check-eq? (chip-8-pc chip) load-address)
             (check-eq? (chip-8-reg-v-ref chip #xA) #x42))

  (test-case "FX0A stores a newly pressed key and continues"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x0A))
             (chip-8-key-pressed! chip #xB)

             (step chip)

             (check-eq? (chip-8-pc chip) (+ load-address 2))
             (check-eq? (chip-8-reg-v-ref chip #xA) #xB)
             (check-false (chip-8-pending-keypress chip)))

  (test-case "FX0A accepts CHIP-8 key zero"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x0A))
             (chip-8-key-pressed! chip #x0)

             (step chip)

             (check-eq? (chip-8-pc chip) (+ load-address 2))
             (check-eq? (chip-8-reg-v-ref chip #xA) #x0))

  (test-case "A held key's auto-repeat does not create another keypress"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xF1 #x0A
                                    #xF2 #x0A))
             (chip-8-key-pressed! chip #x5)
             (step chip)

             (chip-8-key-pressed! chip #x5)
             (check-false (chip-8-pending-keypress chip))
             (step chip)

             (check-eq? (chip-8-reg-v-ref chip #x1) #x5)
             (check-eq? (chip-8-pc chip) (+ load-address 2)))

  (test-case "A key release does not satisfy FX0A"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #x60 #x00
                                    #xF1 #x0A))
             (chip-8-key-pressed! chip #x5)
             (step chip) ; discard the press before FX0A starts waiting
             (chip-8-key-released! chip #x5)

             (step chip)

             (check-eq? (chip-8-pc chip) (+ load-address 2))
             (check-eq? (chip-8-reg-v-ref chip #x1) #x0))

  (test-case "load-rom resets held keys and pending keypresses"
             (define chip (make-chip-8))
             (load-rom! chip minimal-rom)
             (chip-8-key-pressed! chip #x0)
             (chip-8-key-pressed! chip #x5)

             (load-rom! chip minimal-rom)

             (check-equal? (chip-8-keypad chip) (make-vector 16 #f))
             (check-false (chip-8-pending-keypress chip)))

  (test-case "FX07 stores the delay timer in VX"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x07))
             (set-chip-8-delay-timer! chip #x42)

             (step chip)

             (check-eq? (chip-8-reg-v-ref chip #xA) #x42))

  (test-case "FX15 sets the delay timer from VX"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x15))
             (chip-8-reg-v-set! chip #xA #x42)

             (step chip)

             (check-eq? (chip-8-delay-timer chip) #x42))

  (test-case "FX18 sets the sound timer from VX"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x18))
             (chip-8-reg-v-set! chip #xA #x42)

             (step chip)

             (check-eq? (chip-8-sound-timer chip) #x42))

  (test-case "tick decrements active timers and requests a beep"
             (define chip (make-chip-8))
             (set-chip-8-delay-timer! chip 2)
             (set-chip-8-sound-timer! chip 1)
             (define beep-count 0)

             (tick! chip (lambda () (set! beep-count (add1 beep-count))))
             (check-eq? (chip-8-delay-timer chip) 1)
             (check-eq? (chip-8-sound-timer chip) 0)
             (check-eq? beep-count 1)

             (tick! chip (lambda () (set! beep-count (add1 beep-count))))
             (check-eq? (chip-8-delay-timer chip) 0)
             (check-eq? (chip-8-sound-timer chip) 0)
             (check-eq? beep-count 1))

  (test-case "FX1E adds VX to I without affecting VF"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x1E))
             (set-chip-8-i! chip #xABC)
             (chip-8-reg-v-set! chip #xA #x10)
             (chip-8-reg-v-set! chip #xF #x7B)

             (step chip)

             (check-eq? (chip-8-i chip) #xACC)
             (check-eq? (chip-8-reg-v-ref chip #xF) #x7B)

             (define overflow-chip (make-chip-8))
             (load-rom! overflow-chip (bytes #xFA #x1E))
             (set-chip-8-i! overflow-chip #xFFF)
             (chip-8-reg-v-set! overflow-chip #xA #x01)
             (chip-8-reg-v-set! overflow-chip #xF #x7B)

             (step overflow-chip)

             (check-eq? (chip-8-i overflow-chip) #x1000)
             (check-eq? (chip-8-reg-v-ref overflow-chip #xF) #x7B))

  (test-case "FX1E optionally sets VF on address-range overflow"
             (define no-overflow-chip (make-chip-8))
             (load-rom! no-overflow-chip (bytes #xFA #x1E))
             (set-chip-8-i! no-overflow-chip #x100)
             (chip-8-reg-v-set! no-overflow-chip #xA #x10)
             (chip-8-reg-v-set! no-overflow-chip #xF #x7B)

             (define overflow-chip (make-chip-8))
             (load-rom! overflow-chip (bytes #xFF #x1E))
             (set-chip-8-i! overflow-chip #xFFF)
             (chip-8-reg-v-set! overflow-chip #xF #x02)

             (parameterize ([fx1e-sets-vf? #t])
               (step no-overflow-chip)
               (step overflow-chip))

             (check-eq? (chip-8-i no-overflow-chip) #x110)
             (check-eq? (chip-8-reg-v-ref no-overflow-chip #xF) #x00)
             ;; FF1E adds the old VF value (2) before VF becomes the flag (1).
             (check-eq? (chip-8-i overflow-chip) #x1001)
             (check-eq? (chip-8-reg-v-ref overflow-chip #xF) #x01))

  (test-case "FX29 points I at the font sprite selected by VX"
             (for ([digit '(#x0 #x7 #xF)])
               (define chip (make-chip-8))
               (load-rom! chip (bytes #xFA #x29))
               (chip-8-reg-v-set! chip #xA digit)

               (step chip)

               (define offset (* font-size digit))
               (check-eq? (chip-8-i chip) (+ font-address offset))
               (check-equal?
                (subbytes (chip-8-memory chip)
                          (chip-8-i chip)
                          (+ (chip-8-i chip) font-size))
                (subbytes default-font offset (+ offset font-size)))))

  (test-case "FX33 stores the decimal digits of VX at I"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x33))
             (set-chip-8-i! chip #x300)
             (chip-8-reg-v-set! chip #xA 156)
             (bytes-copy! (chip-8-memory chip) #x2FF
                          (bytes #xEE #xEE #xEE #xEE #xEE))

             (step chip)

             (check-equal? (subbytes (chip-8-memory chip) #x2FF #x304)
                           (bytes #xEE 1 5 6 #xEE))
             (check-eq? (chip-8-i chip) #x300)
             (check-eq? (chip-8-reg-v-ref chip #xA) 156))

  (test-case "FX55 stores V0 through VX and increments I"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x55))
             (set-chip-8-i! chip #x300)
             (bytes-copy! (chip-8-reg-v chip) 0
                          (bytes #x10 #x20 #x30 #x40 #x50 #x60
                                 #x70 #x80 #x90 #xA0 #xB0))
             (bytes-set! (chip-8-memory chip) #x30B #xEE)

             (step chip)

             (check-equal? (subbytes (chip-8-memory chip) #x300 #x30C)
                           (bytes #x10 #x20 #x30 #x40
                                  #x50 #x60 #x70 #x80
                                  #x90 #xA0 #xB0 #xEE))
             (check-eq? (chip-8-i chip) #x30B))

  (test-case "FX55 can leave I unchanged"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xF2 #x55))
             (set-chip-8-i! chip #x300)
             (chip-8-reg-v-set! chip #x0 #x12)
             (chip-8-reg-v-set! chip #x1 #x34)
             (chip-8-reg-v-set! chip #x2 #x56)
             (bytes-set! (chip-8-memory chip) #x303 #xEE)

             (parameterize ([load-store-increments-i? #f])
               (step chip))

             (check-equal? (subbytes (chip-8-memory chip) #x300 #x304)
                           (bytes #x12 #x34 #x56 #xEE))
             (check-eq? (chip-8-i chip) #x300))

  (test-case "FX65 loads V0 through VX and increments I"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xFA #x65))
             (set-chip-8-i! chip #x300)
             (bytes-fill! (chip-8-reg-v chip) #xEE)
             (bytes-copy! (chip-8-memory chip) #x300
                          (bytes #x10 #x20 #x30 #x40 #x50 #x60
                                 #x70 #x80 #x90 #xA0 #xB0))

             (step chip)

             (check-equal? (chip-8-reg-v chip)
                           (bytes #x10 #x20 #x30 #x40
                                  #x50 #x60 #x70 #x80
                                  #x90 #xA0 #xB0 #xEE
                                  #xEE #xEE #xEE #xEE))
             (check-eq? (chip-8-i chip) #x30B))

  (test-case "FX65 can leave I unchanged"
             (define chip (make-chip-8))
             (load-rom! chip (bytes #xF2 #x65))
             (set-chip-8-i! chip #x300)
             (chip-8-reg-v-set! chip #x3 #xEE)
             (bytes-copy! (chip-8-memory chip) #x300
                          (bytes #x12 #x34 #x56))

             (parameterize ([load-store-increments-i? #f])
               (step chip))

             (check-eq? (chip-8-reg-v-ref chip #x0) #x12)
             (check-eq? (chip-8-reg-v-ref chip #x1) #x34)
             (check-eq? (chip-8-reg-v-ref chip #x2) #x56)
             (check-eq? (chip-8-reg-v-ref chip #x3) #xEE)
             (check-eq? (chip-8-i chip) #x300))
  )
