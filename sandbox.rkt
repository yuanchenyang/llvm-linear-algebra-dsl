#lang racket

(define (power x n)
  (if (= n 0)
      '1
      `(* ,x ,(power x (- n 1)))))
