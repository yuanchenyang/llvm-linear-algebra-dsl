#lang racket

(require "nodes.rkt")
(require racket/pretty)

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

(define (replace-symbol-for-node tree replacer)
  (match-let ([(for-node loopvar start end incr body) tree])
    (for-node (replacer loopvar)
              (replacer start)
              (replacer end)
              (replacer incr)
              (map replacer body))))

(define (replace-symbol-assign tree replacer)
  (match-let ([(assign target value) tree])
    (assign (replacer target) (replacer value))))

(define (replace-symbol-array-reference tree replacer)
  (match-let ([(array-reference arr index) tree])
    ;; TODO: Hack wrap arr in symbol so it's recursively handled
    (array-reference (replacer (symbol arr)) (replacer index))))

(define (replace-symbol-binop tree op replacer)
  (match-let ([(binop op1 op2) tree])
    (op (replacer op1) (replacer op2))))

(define (replace-symbol to-replace new)
  (define (replacer tree)
    (cond [(for-node? tree) (replace-symbol-for-node tree replacer)]
          [(symbol? tree) (if (equal? (symbol-name tree) to-replace)
                            (symbol new) tree)]
          [(assign? tree) (replace-symbol-assign tree replacer)]
          [(array-reference? tree) (replace-symbol-array-reference tree replacer)]
          [(add? tree) (replace-symbol-binop tree add replacer)]
          [(mul? tree) (replace-symbol-binop tree mul replacer)]
          [(num? tree) tree]
          [else (error tree)]))
  replacer)

(define (fuse loop1 loop2)
  (match-let* ([(for-node loopvar start end incr body) loop1]
               [(for-node loopvar2 start2 end2 incr2 body2) loop2]
               [body-replaced (map (replace-symbol (symbol-name loopvar) (symbol-name loopvar2)) body)])
    (struct-copy for-node loop2 [body (foldr fuse-loops '() (append body-replaced body2))])))

(define (fuse-loops prev result)
  (cond [(null? result) (list prev)]
        [(and (for-node? prev) (for-node? (car result)))
         (cons (fuse prev (car result)) (cdr result))]
        [else (cons prev result)]))

(define (fusion-pass tree)
  (cond [(func-decl? tree) (struct-copy func-decl tree [body (fusion-pass (func-decl-body tree))])]
        [(list? tree) (foldr fuse-loops '() tree)]
        [else (error "Unsupported node type")]))

(provide fusion-pass)

;(pretty-print tree)
;(pretty-print (fusion-pass tree))
