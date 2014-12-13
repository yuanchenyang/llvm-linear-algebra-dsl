#lang racket
(require ffi/unsafe)
(require "nodes.rkt")

(provide (all-defined-out))

;; TODO: add types
(struct matrix
  (id
   rows
   cols
   [contents #:mutable]
   [constant? #:auto #:mutable])
  #:transparent)

(define (make-matrix id rows cols)
  (matrix id rows cols (malloc (_array _int rows cols))))

(define (matrix-ref mat row col)
  (ptr-ref (matrix-contents mat) _int (+ (* row (matrix-cols mat)) col)))


(define (matrix-set! mat row col val)
  (ptr-set! (matrix-contents mat) _int (+ (* row (matrix-cols mat)) col) val))

(define (matrix-load! mat vals)
  (define i 0)
  (for ([row vals])
    (define j 0)
    (for ([val row])
      (matrix-set! mat i j val)
      (set! j (+ j 1)))
    (set! i (+ i 1))))

(define (matrix-display mat)
  (for ([i (matrix-rows mat)])
    (display "(")
    (for ([j (matrix-cols mat)])
      (display (matrix-ref mat i j))
      (if (not (= j (- (matrix-cols mat) 1))) (display ", ") #f))
    (displayln ")")))

;; TODO: add type-checking
(define (mat-block? val)
  (or (matrix? val) (block? val)))

(define (get-mat-id val)
  (if (matrix? val)
      (matrix-id val)
      (block-return val)))

(define (get-stmts val)
  (if (matrix? val)
      '()
      (block-stmts val)))

(define (make-constant-matrix lst)
  (let* ([rows (length lst)]
         [cols (length (car lst))]
         [mat  (make-matrix "const" rows cols)])
    (set-matrix-constant?! mat #t)
    (matrix-load! mat lst)
    mat))
