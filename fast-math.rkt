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
   [constant? #:auto #:mutable]
   [contents #:auto #:mutable])
  #:transparent)

;; TODO: add type-checking
(define (mat-block? val)
  (or (matrix? val) (block? val)))

(define (get-mat-id val)
  ;(assert mat-block? val)
  (if (matrix? val)
      (matrix-id val)
      (block-return val)))

(define (get-stmts val)
  ;(assert mat-block? val)
  (if (matrix? val)
      '()
      (block-stmts val)))

(define (make-constant-matrix lst)
  (letrec ([rows (length lst)]
           [cols (length (car lst))]
           [mat  (matrix rows cols)])
    (set-matrix-constant?! mat #t)
    (set-matrix-contents!  mat (apply append lst))
    mat))

(define (+. a b)
  ;; Adds two values together, the following types are supported:
  ;; (Number a) => (Matrix a) -> (Matrix a) -> (Matrix a)
  ;; (Number a) => a -> a -> a
  (cond [(and (number? a) (number? b))
         (add (num a) (num b))]
        [(and (mat-block? a) (mat-block? b))
         (letrec
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
 (let ([a (matrix "a" 3 4)]
       [b (matrix "b" 3 4)])
   (+. a (+. b a))))


(require rackunit)

;; (let ([a (matrix [[1 2 3] [1 2 3]])]
;;       [b (matrix [[1 2 3] [1 2 3]])])
;;   (test-begin
;;    "Test matrix-matrix plus"
;;    (let ([a-plus-b (+ a b)])
;;      (check-pred call? a-plus-b))))
