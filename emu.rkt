#lang racket

(struct chip-8 (memory pc framebuffer) #:transparent #:mutable)

(define memory-size #x1000)
(define load-address #x200)
(define screen-height 32)
(define screen-width 64)

(define (make-chip-8)
  (chip-8
   ;; 4096 (0x1000) memory locations, all of which are 8 bits
   (make-bytes memory-size)
   ;; program counter points at the current instruction in memory
   load-address
   ;; monochrome screen of 64 × 32 pixels,
   ;; top-left corner of the screen is assigned coordinates of (0x00, 0x00)
   ;; and the bottom-right is assigned (0x3F, 0x1F).
   (make-bytes (* screen-height screen-width))
   ))

(define (load-rom! c rom)
  (bytes-fill! (chip-8-memory c) 0)
  (bytes-fill! (chip-8-framebuffer c) 0)
  (bytes-copy! (chip-8-memory c) load-address rom)
  (set-chip-8-pc! c load-address))

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

(define (step c [request-redraw void])
  ;; FETCH
  (define opcode (fetch-opcode c))
  (set-chip-8-pc! c (+ 2 (chip-8-pc c)))
  ;; DECODE & EXECUTE
  (match-define (list family X Y N NN NNN) (decompose-opcode opcode))
  ;; https://github.com/mattmikolay/chip-8/wiki/CHIP%E2%80%908-Instruction-Set
  (match (list family X Y N)
    ; 00E0 Clear the screen
    [(list #x0 #x0 #xE #x0) (bytes-fill! (chip-8-framebuffer c) 0) (request-redraw)]
    ; 00EE Return from a subroutine
    [(list #x0 #x0 #xE #xE) (error "[TODO] Opcode 0x00ee not implemented!")]
    ; 0NNN Execute machine language subroutine - unsupported by this emulator
    [(list #x0 _ _ _) (error "Subroutines written in machine language are unsupported")]
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
  (load-rom! chip test-rom)
  (step chip))

(module+ test
  (require rackunit)

  (test-case "constructor"
             (define chip (make-chip-8))
             (check-equal? (chip-8-memory chip) (make-bytes memory-size 0)))

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
  )
