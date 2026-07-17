#lang racket

(struct chip-8 (memory pc) #:transparent #:mutable)

(define memory-size #x1000)
(define load-address #x200)

(define (make-chip-8)
  (chip-8 (make-bytes memory-size) ;; 4096 (0x1000) memory locations, all of which are 8 bits
          load-address ;; program counter points at the current instruction in memory
          ))

(define (load-rom! c rom)
  (bytes-fill! (chip-8-memory c) 0)
  (bytes-copy! (chip-8-memory c) load-address rom)
  (set-chip-8-pc! c load-address))

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
  )
