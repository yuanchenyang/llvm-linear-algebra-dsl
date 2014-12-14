#lang racket

(require racket/generic)

(provide (all-defined-out))

(define-generics node
  [node-children node]
  [node-dependencies node]
  [node-map fn node]
)

(define (collect-uniq l)
  (list (list->mutable-set (apply append (map car l)))
        (list->mutable-set (apply append (map cadr l)))))

(struct for-node
        (loop-var init end incr body)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(for-node loopvar start end incr body) node])
             (append (list loopvar start end incr) body)))
         (define (node-map fn node)
           (map (lambda (n) (node-map fn n)) (node-children node)))
         (define (node-accesses node)
           (let ([children (node-children node)])
             (collect-uniq (map node-accesses children))))])

(struct assign
        (target value)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(assign target value) node])
             (list target value)))
         (define (node-accesses node)
           (match-let* ([(assign target value) node]
                        [children (node-children node)]
                        [(list reads writes) (collect-uniq (map node-accesses children))])
             (set-add! writes target)
             (list reads writes)))])
(struct binop
        (op1 op2)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(binop op1 op2) node])
             (list op1 op2)))
         (define (node-accesses node)
           (let ([children (node-children node)])
             (collect-uniq (map node-accesses children))))])
(struct add binop () #:transparent)
(struct mul binop () #:transparent)
(struct lt binop  () #:transparent)
(struct array-reference
        (arr index)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(array-reference arr index) node])
             (list arr index)))
         (define (node-accesses node)
           (match-let* ([(array-reference arr index) node]
                        [children (node-children node)]
                        [(list reads writes) (collect-uniq (map node-accesses children))])
             (set-add! reads arr)
             (list reads writes)))])
(struct num
        (value)
        #:transparent
        #:methods gen:node
        [(define (node-children node) '())
         (define (node-accesses node) (list (mutable-set) (mutable-set)))])
(struct symbol
        (name)
        #:transparent
        #:methods gen:node
        [(define (node-children node)
           (match-let ([(symbol name) node])
             (list name)))
         (define (node-accesses node)
           (match-let* ([(symbol name) node])
           (list (mutable-set symbol) (mutable-set))))])

(struct func-decl (ret-type name params body)      #:transparent)
(struct return    (target)                         #:transparent)
(struct block     (stmts return)                   #:transparent)
(struct param     (name type)                      #:transparent)
(struct allocate  (target type rows cols)          #:transparent)


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
