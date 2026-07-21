#lang racket

(provide make-chip-8
         load-rom!
         step
         chip-8-pixel-ref
         screen-width
         screen-height)

(struct chip-8 (memory pc i reg-v stack sp framebuffer) #:transparent #:mutable)

(define memory-size #x1000)
(define stack-size 12)
(define load-address #x200)
(define screen-height 32)
(define screen-width 64)

;; ambiguous instructions!
; The original COSMAC VIP allows I to exceed 0xFFF and leaves VF unchanged.
; Some later interpreters set VF on overflow: CHIP-8 interpreter for Amiga
; behaved this way. At least one known game, Spacefight 2091!, relies on it
(define fx1e-sets-vf? (make-parameter #f))

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

(define (load-rom! c rom)
  ; reset emulator state
  (bytes-fill! (chip-8-memory c) 0)
  (bytes-copy! (chip-8-memory c) load-address rom)
  (set-chip-8-pc! c load-address)
  (set-chip-8-i! c 0)
  (bytes-fill! (chip-8-reg-v c) 0)
  (vector-fill! (chip-8-stack c) 0)
  (set-chip-8-sp! c 0)
  (bytes-fill! (chip-8-framebuffer c) 0))

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
    ; 9XY0 Skip the following instruction if the value of register VX is not equal to the value of register VY
    [(list #x9 _ _ #x0) (unless (= (chip-8-reg-v-ref c X) (chip-8-reg-v-ref c Y)) (skip! c))]
    ; ANNN Store memory address NNN in register I
    [(list #xA _ _ _) (set-chip-8-i! c NNN)]
    ; CXNN Set VX to a random number with a mask of NN
    [(list #xC _ _ _) (chip-8-reg-v-set! c X (bitwise-and (random #x100) NN))]
    ; DXYN Draw a sprite at position VX, VY with N bytes of sprite data
    ;      starting at the address stored in I.  Set VF to 01 if any set
    ;      pixels are changed to unset, and 00 otherwise
    [(list #xD _ _ _) (chip-8-draw-sprite! c X Y N) (request-redraw)]
    ; FX1E Add the value stored in register VX to register I
    [(list #xF _ #x1 #xE)
     (define new-i (+ (chip-8-i c) (chip-8-reg-v-ref c X)))
     (set-chip-8-i! c new-i)
     (when (fx1e-sets-vf?) (chip-8-reg-v-set! c #xF (if (> new-i #xFFF) 1 0)))]
    [else (error (string-append "Unknown opcode 0x" (~r opcode #:base 16 #:min-width 4 #:pad-string "0")))])
  )

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
  (for ([i (in-naturals)])
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
    (check-equal? (subbytes mem 0 load-address) (make-bytes load-address))
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
  )
