#lang racket
#|
  (matrix [[expr ...+] ...+])

Entrywise Ops
  (+ M N) -> (Matrix Number)
  (- M N) -> (Matrix Number)
  (* M N) -> (Matrix Number)
  (/ M N) -> (Matrix Number)
    M: (Matrix Number)
    N: (Matrix Number)

  (+ M N) -> (Matrix Number)
  (- M N) -> (Matrix Number)
  (* M N) -> (Matrix Number)
  (/ M N) -> (Matrix Number)
    M: (Matrix Number)
    N: Number

  (+ M N) -> (Matrix Number)
  (- M N) -> (Matrix Number)
  (* M N) -> (Matrix Number)
  (/ M N) -> (Matrix Number)
    M: Number
    N: (Matrix Number)

  (+ M N) -> Number
  (- M N) -> Number
  (* M N) -> Number
  (/ M N) -> Number
    M: Number
    N: Number

Special
  (map f M) -> (Matrix Number)
    f: (Number -> Number)
    M: (Matrix Number)
  (map f M N ...) -> (Matrix Number)
    f: (Number Number ... -> Number)
    M: (Matrix Number)
    N: (Matrix Number)

  (transpose M) -> (Matrix Number)
    M: (Matrix Number)
|#
; (require math/array)
; (require math/matrix)
(require ffi/unsafe)
(require "nodes.rkt")
(require "utils.rkt")
(require racket/pretty)

(struct symbol-ref (id)  #:transparent)
(struct num-ref (id)     #:transparent)
(struct call (func args) #:transparent)

;; (define (contains-matrix args)
;;   (if (null? args) #f
;;       (let ([a (car args)])
;;         (or (and (array? a) (matrix? a))
;; 	       (contains-matrix (cdr args))))))

;; TODO: add types
(struct matrix
  (id
   rows
   cols
   contents
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


(define (+. a b)
  ;; Adds two values together, the following types are supported:
  ;; (Number a) => (Matrix a) -> (Matrix a) -> (Matrix a)
  ;; (Number a) => a -> a -> a
  (cond [(and (number? a) (number? b))
         (add (num a) (num b))]
        [(and (mat-block? a) (mat-block? b))
         (let*
           ([i (gen-unique-symbol)]
            [j (gen-unique-symbol)]
            [target (gen-unique-symbol)]
            [rows (matrix-rows a)]
            [cols (matrix-cols a)]
            [index (add j (mul rows i))]
            [node
             (for-node (symbol i) (num 0) (num rows) (num 1)
               (for-node (symbol j) (num 0) (num cols) (num 1)
                 (assign (array-reference target index)
                         (add (array-reference (get-mat-id a) index)
                              (array-reference (get-mat-id b) index)))))])
           (block (list (get-stmts a) (get-stmts b) node)
                  target))]
        [else (error "Invalid arguments to add!")]))

(pretty-print
 (let ([a (make-matrix "a" 3 4)]
       [b (make-matrix "b" 3 4)])
   (+. a (+. b a))))


(require rackunit)
