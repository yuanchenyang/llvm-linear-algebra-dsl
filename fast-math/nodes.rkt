#lang racket

(require racket/generic)

(provide (all-defined-out))

(define-generics node
  [node-children node]
  [node-dependencies node]
)

(struct for-node
        (loop-var init end incr body)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(for-node loopvar start end incr body) node])
             (append (list loopvar start end incr) body)))])
(struct assign
        (target value)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(assign target value) node])
             (list target value)))])
(struct binop
        (op1 op2)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(binop op1 op2) node])
             (list op1 op2)))])
(struct add binop () #:transparent)
(struct mul binop () #:transparent)
(struct lt binop  () #:transparent)
(struct array-reference
        (arr index)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(array-reference arr index) node])
             (list arr index)))])
(struct num
        (value)
        #:transparent
        #:methods gen:node
        [(define (node-children node) '())])
(struct symbol
        (name)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(symbol name) node])
             (list name)))])
(struct func-decl (name params body) #:transparent)
(struct return    (target)           #:transparent)
(struct block     (stmts return)     #:transparent)
(struct param     (name type)        #:transparent)



(define int-ptr 0)
(define mat int-ptr)
(define int 1)

(define (for-loop loopvar start end incr body)
  (for-node (if (symbol? loopvar) loopvar (symbol loopvar))
            (num start)
            (num end)
            (num incr)
            body))

;; A for loop wrapped in a list, basically a block without any return statementss
(define (for-block loopvar start end incr body)
  (list (for-loop loopvar start end incr body)))
