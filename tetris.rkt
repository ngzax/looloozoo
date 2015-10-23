#lang racket

(provide block-touch-top? blocks-touch-top?)

(define (block-touch-top? s) true)

(define (blocks-touch-top? s)
 (cond [(empty? s) false]
       [(cons? s) (if (block-touch-top? (first s))
                      true
                      (blocks-touch-top? (rest s)))]))
