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

(require "nodes.rkt")
(require "matrix.rkt")
(require "utils.rkt")
(require "transforms.rkt")
(require racket/pretty)

(provide +. convolve. define-optimized)

(define (+. a b)
  ;; Adds two values together, the following types are supported:
  ;; (Number a) => (Matrix a x y) -> (Matrix a x y) -> (Matrix a x y)
  ;; (Number a) => a -> a -> a
  (cond [(and (number? a) (number? b))
         (add (num a) (num b))]
        [(and (mat-block? a) (mat-block? b))
         (let*
           ([i      (gen-unique-symbol)]
            [j      (gen-unique-symbol)]
            [target (gen-unique-symbol)]
            [rows   (matrix-rows a)]
            [cols   (matrix-cols a)]
            [index  (num (add j (mul rows i)))]
            [node
             (for-block i 0 rows 1
               (for-block j 0 cols 1
                 (list (assign (array-reference target index)
                               (add (array-reference (get-mat-id a) index)
                                    (array-reference (get-mat-id b) index))))))])
           (block (append (get-stmts a) (get-stmts b) node)
                  target))]
        [else (error "Invalid type of arguments to add!")]))

(define (convolve. a b)
  ;; Convolves two matrices together.
  ;; (Number a) => (Matrix a x1 y1) -> (Matrix a x2 y1) -> (Matrix a x1 y1)
  (if (and (mat-block? a) (mat-block? b))
      (let* ([x  (gen-unique-symbol)] [y  (gen-unique-symbol)]
             [xx (gen-unique-symbol)] [yy (gen-unique-symbol)]
             [target (gen-unique-symbol)]
             [xa (matrix-cols a)] [ya (matrix-rows a)]
             [xb (matrix-cols b)] [yb (matrix-rows b)]
             [padx (/ xb 2)]      [pady (/ yb 2)]
             ;; xx + x + xa * (y + yy)
             [in-index     (num (add xx (add x (mul xa (add y yy)))))]
             ;; x + xa * y
             [out-index    (num (add x  (mul y xa)))]
             ;; xx + xb * yy
             [kern-index (num (add xx (mul xb yy)))]
             [in   (array-reference (get-mat-id a) in-index)]
             [out  (array-reference target out-index)]
             [kern (array-reference (get-mat-id b) kern-index)]
             [node (for-block y pady (- ya pady) 1
                     (for-block x padx (- xa padx) 1
                       (for-block yy (- pady) (+ pady 1) 1
                         (for-block xx (- padx) (+ padx 1) 1
                           (list (assign out (add out (mul in kern))))))))])
        (block (append (get-stmts a) (get-stmts b) node)
               target))
      (error "Invalid type of arguments to convolve!")))

(define-syntax define-optimized
  (syntax-rules ()
    [(_ (name ret-type (arg type) ...) body)
     (define (name arg ...)
       (let* ([evalb  body]
              [sname  (symbol->string 'name)]
              [stmts  (fusion-pass (block-stmts  evalb))]
              [ret    (block-return evalb)]
              [blk    (append stmts (list (return ret)))]
              [params (list (param (symbol->string 'arg) type) ...)])
         (func-decl ret-type sname params blk)))]))

(define-optimized (test-add mat (a mat) (b mat))
  (+. a (+. b a)))

(let ([a (make-matrix "a" 3 4)]
      [b (make-matrix "b" 3 4)]
      [c (make-matrix "c" 10 10)]
      [d (make-matrix "d" 2 2)])
  (pretty-print (test-add a b))
  (pretty-print (convolve. c d))
  )
