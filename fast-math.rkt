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


(struct symbol-ref (id) #:transparent)
(struct num-ref (id) #:transparent)
(struct call (func args) #:transparent)

(define (contains-matrix args)
  (if (null? args) #f
      (let ([a (car args)])
	(or (and (array? a) (matrix? a))
	    (contains-matrix (cdr args))))))

(define-syntax-rule (+ a b)
  ;; TODO: Typechecking (matrix sizes compatible, unsupported types)
  ;; (+ M N) -> (Matrix Number)
  ;;   M: (Matrix Number)
  ;;   N: (Matrix Number)
  ;; (+ M N) -> (Matrix Number)
  ;;   M: Number
  ;;   N: (Matrix Number)
  ;; (+ M N) -> (Matrix Number)
  ;;   M: (Matrix Number)
  ;;   N: Number
  ;; (+ M N) -> Number
  ;;   M: Number
  ;;   N: Number
  (if (contains-matrix (list a b))
      (call (symbol-ref "matrix+")
	    '((symbol-ref a) (symbol-ref b)))
      (call (symbol-ref "+")
	    '((symbol-ref a) (symbol-ref b)))))


(require rackunit)

(let ([a (matrix [[1 2 3] [1 2 3]])]
      [b (matrix [[1 2 3] [1 2 3]])])
  (test-begin
   "Test matrix-matrix plus"
   (let ([a-plus-b (+ a b)])
     (check-pred call? a-plus-b))))
