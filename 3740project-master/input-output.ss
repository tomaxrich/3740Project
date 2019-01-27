#lang racket
(define varhash (make-weak-hash))

(define (input var)
  (define h1 var)
  (define h2 (read))
  (hash-set! varhash h1 h2)
  void)

(define (output var)
  (define h3 var)
  (hash-ref varhash h3))

(provide input)
(provide output)
(provide varhash)
