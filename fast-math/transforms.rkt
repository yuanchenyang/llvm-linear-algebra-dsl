#lang racket

(require fast-math/nodes)
(require fast-math/matrix)

(provide fusion-pass constant-fold for-unroll loop-compression)

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
              (map folder body)
              pragmas)))

(define (fold-symbol-assign tree folder)
  (match-let ([(assign target value) tree])
    (assign (folder target) (folder value))))

(define (fold-symbol-array-reference tree folder)
  (match-let ([(array-reference arr index) tree])
    ;; TODO: Hack wrap arr in symbol so it's recursively handled
    (let ([a (folder arr)]
          [i (folder index)])
      (if (matrix? a)
          (if (and (matrix-constant? a) (num? i))
              (num (matrix-ref-index a (num-value i)))
              (array-reference (get-mat-id a) i))
          (array-reference a i)))))

(define (fold-symbol-binop tree op op-fn folder)
  (match-let ([(binop op1 op2) tree])
    (let ([a (folder op1)]
          [b (folder op2)])
      (if (and (num? a) (num? b))
          (num (op-fn (num-value a) (num-value b)))
          (op a b)))))

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
  (match-let*
   ([(for-node loopvar start end incr body pragmas) loop1]
    [(for-node loopvar2 start2 end2 incr2 body2 pragmas) loop2]
    [body-folded
     (map (constant-fold
           (make-hash (list (cons (symbol-name loopvar) loopvar2))))
          body)])
   (struct-copy for-node loop2 [body (foldr fuse-loops '() (append body-folded body2))])))

(define (fusable? prev next)
  (and (for-node? prev) (for-node? next)
       (= (num-value (for-node-init prev)) (num-value (for-node-init next)))
       (= (num-value (for-node-end prev)) (num-value (for-node-end next)))
       (= (num-value (for-node-incr prev)) (num-value (for-node-incr next)))
       (member pragma-ignore-loop-deps (for-node-pragmas prev))
       (member pragma-ignore-loop-deps (for-node-pragmas next))))

(define (fuse-loops prev result)
  (cond [(null? result) (list prev)]
        [(fusable? prev (car result))
         (cons (fuse prev (car result)) (cdr result))]
        [else (cons prev result)]))

(define (fusion-pass tree)
  (let ([body (func-decl-body tree)])
    (struct-copy
     func-decl
     tree
     [body (struct-copy
            block
            body
            [stmts (foldr fuse-loops '() (block-stmts body))])])))

(define (for-unroll env loopvar start end incr body [pragmas '()] )
  (flatten
   (for/list ([i (in-range start end incr)])
     (hash-set! env (symbol-name loopvar) (num i))
     (map (constant-fold env)
          body))))

(define (push-until-dependence for-node dependencies so-far)
  (if (or (null? so-far) (member for-node (car dependencies)))
      (cons for-node so-far)
      (cons (car so-far) (push-until-dependence for-node (cdr dependencies) (cdr so-far)))))

(define (loop-compression func)
  (let* ([func-body (func-decl-body func)]
         [statements (block-stmts func-body)]
         [dependencies (node-dependencies func)])
    (struct-copy func-decl func
                 [body (block (for/fold ([so-far '()])
                                  ([index (in-naturals 0)]
                                   [curr (reverse statements)])
                                (if (for-node? curr)
                                    (push-until-dependence curr
                                                           (take-right dependencies index) so-far)
                                    (cons curr so-far))) (block-return func-body))])))


;(pretty-print tree)
;(pretty-print (fusion-pass tree))
