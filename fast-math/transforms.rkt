#lang racket

(require "nodes.rkt")
(require "matrix.rkt")
(require racket/pretty)

(provide fusion-pass constant-fold for-unroll)

(define tree
  (func-decl
   int-ptr
   "func1"
   (list (param "a" int-ptr) (param "b" int-ptr) (param "c" int-ptr) (param "d" int-ptr))
   (list
     (for-loop "u1" 0 2 1
       (list
         (for-loop "u2" 0 2 1
          (list
            (assign
             (array-reference  "c" (add (symbol "u2") (mul (num 2) (symbol "u1"))))
             (add
              (array-reference "b" (add (symbol "u2") (mul (num 2) (symbol "u1"))))
              (array-reference "a" (add (symbol "u2") (mul (num 2) (symbol "u1"))))))))))
     (for-loop "y1" 0 2 1
       (list
         (for-loop "y2" 0 2 1
          (list
            (assign
             (array-reference  "d" (add (symbol "y2") (mul (num 2) (symbol "y1"))))
             (add
              (array-reference "c" (add (symbol "y2") (mul (num 2) (symbol "y1"))))
              (array-reference "a" (add (symbol "y2") (mul (num 2) (symbol "y1"))))))))))
     (return (symbol "d")))))

(define (fold-symbol-for-node tree folder)
  (match-let ([(for-node loopvar start end incr body pragmas) tree])
    (for-node (folder loopvar)
              (folder start)
              (folder end)
              (folder incr)
              pragmas
              (map folder body))))

(define (fold-symbol-assign tree folder)
  (match-let ([(assign target value) tree])
    (assign (folder target) (folder value))))

(define (fold-symbol-array-reference tree folder)
  (match-let ([(array-reference arr index) tree])
    ;; TODO: Hack wrap arr in symbol so it's recursively handled
    (let ([a (folder arr)]
          [i (folder index)])
      (if (and (matrix? a) (matrix-constant? a) (num? i))
          (num (matrix-ref-index a (num-value i)))
          (array-reference a i)))))

(define (fold-symbol-binop tree op op-fn folder)
  (match-let ([(binop op1 op2) tree])
    (let ([a (folder op1)]
          [b (folder op2)])
      (if (and (num? a) (num? b))
          (num (+ (num-value a) (num-value b)))
          (add a b)))))

(define (constant-fold env)
  (define (folder tree)
    (cond [(for-node? tree) (fold-symbol-for-node tree folder)]
          [(symbol? tree)
           (let ([name (symbol-name tree)])
             (if (hash-has-key? env name)
                 (hash-ref env name)
                 tree))]
          [(assign? tree) (fold-symbol-assign tree folder)]
          [(array-reference? tree) (fold-symbol-array-reference tree folder)]
          [(add? tree) (fold-symbol-binop tree add + folder)]
          [(mul? tree) (fold-symbol-binop tree mul * folder)]
          [(num? tree) tree]
          [else (error tree)]))
  folder)

(define (fuse loop1 loop2)
  (match-let* ([(for-node loopvar start end incr body pragmas) loop1]
               [(for-node loopvar2 start2 end2 incr2 body2 pragmas) loop2]
               [body-folded
                (map (constant-fold
                      (make-hash (list (cons (symbol-name loopvar) loopvar2))))
                     body)])
    (struct-copy for-node loop2 [body (foldr fuse-loops '() (append body-folded body2))])))

(define (fuse-loops prev result)
  (cond [(null? result) (list prev)]
        [(and (for-node? prev) (for-node? (car result)))
         (cons (fuse prev (car result)) (cdr result))]
        [else (cons prev result)]))

(define (fusion-pass tree)
  (cond [(func-decl? tree) (struct-copy func-decl tree [body (fusion-pass (func-decl-body tree))])]
        [(list? tree) (foldr fuse-loops '() tree)]
        [else (error "Unsupported node type")]))

(define (for-unroll env loopvar start end incr body [pragmas '()] )
  (for/list ([i (in-range start end incr)])
    (hash-set! env (symbol-name loopvar) (num i))
    ((constant-fold env)
     body)))




;(pretty-print tree)
;(pretty-print (fusion-pass tree))
