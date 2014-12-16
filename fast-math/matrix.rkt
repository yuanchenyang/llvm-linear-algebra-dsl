#lang racket
(require ffi/unsafe)
(require fast-math/nodes)

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
  (matrix id rows cols (malloc (_array _double rows cols))))

(define (make-zero-matrix id rows cols)
  (let ([mat (make-matrix id rows cols)])
    (for ([i (in-range rows)])
      (for ([j (in-range cols)])
        (matrix-set! mat i j 0.0)))
    mat))

(define (make-matrix-with-ptr id rows cols ptr)
  (matrix id rows cols ptr))

(define (matrix-ref mat row col)
  (ptr-ref (matrix-contents mat) _double (+ (* row (matrix-cols mat)) col)))

(define (matrix-ref-index mat index)
  (ptr-ref (matrix-contents mat) _double index))

(define (matrix-set! mat row col val)
  (ptr-set! (matrix-contents mat) _double (+ (* row (matrix-cols mat)) col) val))

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
  (or (matrix? val) (block? val) (matrix-block? val)))

(define (get-mat-id val)
  (cond [(matrix? val) (symbol (matrix-id val))]
        [(matrix-block? val)
         (matrix-block-return val)]
        [else (block-return val)]))

(define (get-stmts val)
  (cond [(matrix? val) '()]
        [(matrix-block? val) (matrix-block-stmts val)]
        [else (block-stmts val)]))

(define (make-constant-matrix name lst)
  (let* ([rows (length lst)]
         [cols (length (car lst))]
         [mat  (make-matrix name rows cols)])
    (set-matrix-constant?! mat #t)
    (for ([row lst]
          [y   (in-naturals 0)])
      (for ([val row]
            [x   (in-naturals 0)])
        (matrix-set! mat y x val)))
    mat))
