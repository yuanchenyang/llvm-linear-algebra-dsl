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
(require math/array)
(require math/matrix)


(struct symbol-ref (id)  #:transparent)
(struct num-ref (id)     #:transparent)
(struct call (func args) #:transparent)

(define (contains-matrix args)
  (if (null? args) #f
      (let ([a (car args)])
	(or (and (array? a) (matrix? a))
	    (contains-matrix (cdr args))))))

;; TODO: add types
(struct matrix
  (rows
   cols
   [constant? #:auto #:mutable]
   [contents #:auto #:mutable])
  #:transparent)

(define (make-constant-matrix lst)
  (letrec ([rows (length lst)]
           [cols (length (car lst))]
           [mat  (matrix rows cols)])
    (set-matrix-constant?! mat #t)
    (set-matrix-contents!  mat (apply append lst))
    mat))

(make-constant-matrix '((1 2 3) (4 5 6)))

(define-syntax-rule (+ a b)
  ;; Adds two values together, the following types are supported:
  ;; (Number a) => (Matrix a) -> (Matrix a) -> (Matrix a)
  ;; (Number a) => a -> a -> a
  (cond [(and (number? a) (number? b))
         ()
         ]
        [(and (matrix? a) (matrix? b))]


  (if (contains-matrix (list a b))
      (call (symbol-ref "matrix+")
	    (list (symbol-ref a) (symbol-ref b)))
      (call (symbol-ref "+")
	    (list (symbol-ref a) (symbol-ref b)))))




(require rackunit)

(let ([a (matrix [[1 2 3] [1 2 3]])]
      [b (matrix [[1 2 3] [1 2 3]])])
  (test-begin
   "Test matrix-matrix plus"
   (let ([a-plus-b (+ a b)])
     (check-pred call? a-plus-b))))
