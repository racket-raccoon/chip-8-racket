#lang racket

(struct chip-8 (memory) #:transparent #:mutable)

(define (make-chip-8)
  (chip-8 (make-bytes (* 4 1024))))

(module+ test
  (require rackunit)

  (test-case "constructor"
             (define chip (make-chip-8))
             (check-eq? (bytes-length (chip-8-memory chip)) #x1000))
  )
