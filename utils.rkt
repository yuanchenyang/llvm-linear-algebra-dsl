#lang racket

(define uniq 0)

(define (gen-unique-num)
  (set! uniq (+ 1 uniq))
  uniq)

(define (gen-unique-symbol)
  (string-append "#u" (number->string (gen-unique-num))))

(provide gen-unique-num
         gen-unique-symbol)
