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

(require racket/pretty)

(require fast-math/nodes)
(require fast-math/matrix)
(require fast-math/utils)
(require fast-math/transforms)
(require fast-math/backend)

(provide +. -. *. /.  convolve. define-optimized)

(define (binop-factory op)
  (lambda (a b)
    ;; Adds two values together, the following types are supported:
    ;; (Number a) => (Matrix a x y) -> (Matrix a x y) -> (Matrix a x y)
    ;; (Number a) => a -> a -> a
    (cond [(and (number? a) (number? b))
           (op (num a) (num b))]
          [(and (mat-block? a) (mat-block? b))
           (let* ([i      (gen-unique-symbol)] [j (gen-unique-symbol)]
                  [target (gen-unique-symbol)]
                  [rows   (if (matrix-block? a)
                              (matrix-block-rows a)
                              (matrix-rows a))]
                  [cols  (if (matrix-block? a)
                             (matrix-block-cols a)
                             (matrix-cols a))]
                  [nrows  (num rows)]      [ncols (num cols)]
                  [index  (add j (mul ncols i))]
                  [node
                   (for-block
                    i 0 rows 1
                    (for-block
                     j 0 cols 1
                     (list (assign (array-reference target index)
                                   (op (array-reference (get-mat-id a) index)
                                       (array-reference (get-mat-id b) index))))
                     (list pragma-ignore-loop-deps))
                    (list pragma-ignore-loop-deps))])
             (matrix-block rows cols
                           (append (get-stmts a)
                                   (get-stmts b)
                                   (list (allocate target mat rows cols))
                                   node)
                           target))]
          [else (error "Invalid type of arguments to binop!")])))

(define +. (binop-factory add))
(define -. (binop-factory sub))
(define *. (binop-factory mul))
(define /. (binop-factory div))

(define (convolve. a b)
  ;; Convolves two matrices together.
  ;; (Number a) => (Matrix a x1 y1) -> (Matrix a x2 y1) -> (Matrix a x1 y1)
  (if (and (mat-block? a) (matrix? b))
      (let* ([x  (gen-unique-symbol)] [y  (gen-unique-symbol)]
             [xx (gen-unique-symbol)] [yy (gen-unique-symbol)]
             [target (gen-unique-symbol)]
             [xa (matrix-cols a)] [ya (matrix-rows a)]
             [xb (matrix-cols b)] [yb (matrix-rows b)]
             [nxa (num xa)] [nxb (num xb)]
             [padx (quotient xb 2)]      [pady (quotient yb 2)]
             [npadx (num padx)]          [npady (num pady)]
             ;; xx + x + nxa * (y + yy)
             [in-index     (add xx (add x (mul nxa (add y yy))))]
             ;; x + nxa * y
             [out-index    (add x  (mul y nxa))]
             ;; xx + nxb * yy
             [kern-index (add (add npadx xx) (mul nxb (add npady yy)))]
             [in   (array-reference (get-mat-id a) in-index)]
             [out  (array-reference target out-index)]
             [kern (array-reference (get-mat-id b) kern-index)]
             [env (make-hash (list (cons (matrix-id b) b)))]
             [node (for-block y pady (- ya pady) 1
                     (for-block x padx (- xa padx) 1
                       (for-unroll env yy (- pady) (+ pady 1) 1
                         (for-unroll env xx (- padx) (+ padx 1) 1
                           (list (assign out (add out (mul in kern))))))))])
        (block (append
                (get-stmts a)
                (list (allocate target mat ya xa))
                node)
               target))
      (error "Invalid type of arguments to convolve!")))

(define-syntax define-optimized
  (syntax-rules ()
    [(_ (name ret-type (arg type) ...) body)
     (define (name arg ...)
       (let* ([evalb    body]
              [sname    (symbol->string 'name)]
              ;; [stmts   (block-stmts  evalb)]
              [stmts    (if (matrix-block? evalb)
                            (matrix-block-stmts evalb)
                            (block-stmts evalb))]
              [ret      (if (matrix-block? evalb)
                            (matrix-block-return evalb)
                            (block-return evalb))]
              [blk      (block stmts (return ret))]
              [params   (list (param (if (matrix? arg) (matrix-id arg)
                                         (symbol->string 'arg)) type) ...)]
              [tree (loop-compression (func-decl ret-type sname params blk))]
              [compiled (do-math tree)])
         ;;compiled))]))
         (compiled arg ...)))]))
