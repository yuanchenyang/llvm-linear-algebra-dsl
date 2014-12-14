#lang racket

(require "nodes.rkt")

(define uniq 0)

(define (gen-unique-num)
  (set! uniq (+ 1 uniq))
  uniq)

(define (gen-unique-name)
  (string-append "u" (number->string (gen-unique-num))))

(define (gen-unique-symbol)
  (symbol (gen-unique-name)))

(define flatten (lambda (x) (apply append x)))

(provide gen-unique-num
         gen-unique-symbol
	 gen-unique-name
         flatten)
