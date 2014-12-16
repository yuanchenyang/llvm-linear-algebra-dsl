#lang racket

(require fast-math/nodes)
(require fast-math/matrix)
(require fast-math/analysis)
(require fast-math/utils)

(provide fusion-pass
         constant-fold
         for-unroll
         loop-compression
         mem-to-reg
         lift-allocates
         loop-sort)

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
          [(div? tree) (fold-symbol-binop tree div / folder)]
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
         [dependencies (node-dependencies func)]
         [stmts (for/fold ([so-far '()])
                    ([index (in-naturals 0)]
                     [curr (reverse statements)])
                  (if (for-node? curr)
                      (push-until-dependence
                       curr (take-right dependencies index) so-far)
                      (cons curr so-far)))])
    (pretty-print dependencies)
    (pretty-print func-body)
    (struct-copy func-decl func
                 [body (block stmts
                              (block-return func-body))])))

(define (do-mem-to-reg live-ins live-outs seen)
  (define (doer statement)
    (cond [(array-reference? statement)
           (let* ([arr-name (symbol-name (array-reference-arr statement))])
             (if (and (not (set-member? live-ins arr-name))
                      (not (set-member? live-outs arr-name)))
                 (let* ([symbol (hash-ref! seen arr-name #f)]
                        [name (if symbol symbol
                                  (begin
                                    (let ([symbol (gen-unique-symbol)])
                                      (hash-set! seen arr-name symbol)
                                      symbol)))])
                   name)
                 statement))]
          [(for-node? statement) (struct-copy
                                  for-node statement [body (map doer (for-node-body statement))])]
          [(assign? statement)
           (match-let ([(assign target value) statement])
             (struct-copy
              assign statement [target (doer target)] [value (doer value)]))]
          [(add? statement)
           (match-let ([(add op1 op2) statement])
             (add (doer op1) (doer op2)))]
          [(mul? statement)
           (match-let ([(mul op1 op2) statement])
             (mul (doer op1) (doer op2)))]
          [else statement]))
  doer)

(define (mem-to-reg-pass bb)
  (match-let ([(basic-block statements live-ins live-outs) bb])
    (map (do-mem-to-reg live-ins live-outs (make-hash)) statements)))

(define (mem-to-reg func)
  (let* ([analyzed (liveness-analyze (build-basic-blocks func))]
         [basic-blocks (block-stmts (func-decl-body analyzed))]
         [promoted (flatten (map mem-to-reg-pass basic-blocks))]
         [ret (last promoted)]
         [stmts (drop-right promoted 1)])
    (struct-copy func-decl func [body (block stmts ret)])))

(define (lift-allocates func)
  (match-let* ([(block stmts ret) (func-decl-body func)])
              (define-values (allocates rest) (partition allocate? stmts))
              (struct-copy func-decl func [body (block (append allocates rest) ret)])))

(define (stmt-compare prev next)
  (match-let* ([(cons prev-dep prev-node) prev]
               [(cons next-dep next-node) next])
              (if (set-member? next-dep prev-node) #t
                  (if (and (for-node? prev-node) (for-node? next-node))
                      (let ([len1 (- (num-value (for-node-end prev-node))
                                     (num-value (for-node-init prev-node)))]
                            [len2 (- (num-value (for-node-end next-node))
                                     (num-value (for-node-init next-node)))])
                        (< len1 len2))
                      #f))))

(define (loop-sort func)
  (match-let* ([(block stmts ret) (func-decl-body func)]
               [dependencies (node-dependencies func)]
               [stmt-deps (map (lambda (x y) (cons x y)) dependencies stmts)]
               [sorted (sort stmt-deps stmt-compare)])
              (struct-copy func-decl func [body (block (map cdr sorted) ret)])))

                                        ;(pretty-print tree)
                                        ;(pretty-print (fusion-pass tree))
