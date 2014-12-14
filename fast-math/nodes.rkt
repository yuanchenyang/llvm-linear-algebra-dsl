#lang racket

(require racket/generic)

(provide
 (except-out
  (all-defined-out)
  collect-uniq))

(define-generics node
  [node-children node]
  [node-accesses node]
  [node-dependencies node]
)

(define (collect-uniq l)
  (list (list->mutable-set (apply append (map (lambda (i) (set->list (car i))) l)))
        (list->mutable-set (apply append (map (lambda (i) (set->list (cadr i))) l)))))

(define (any-a-in-b set-a set-b)
  (ormap (lambda (a) (set-member? set-b a)) (set->list set-a)))

(define (dependent? node-after node-before)
  (match-let ([(list reads-after writes-after) (node-accesses node-after)]
              [(list reads-before writes-before) (node-accesses node-before)])
    (or (any-a-in-b reads-after writes-before)
        (any-a-in-b writes-after reads-before)
        (any-a-in-b writes-after writes-before))))

(define (filter-before filter-func before-func lst)
  (filter filter-func (sequence->list (stop-before lst before-func))))

(struct for-node
        (loop-var init end incr body pragmas)
        #:transparent
        #:methods gen:node
        [(define/generic super-accesses node-accesses)
         (define (node-children node)
           (match-let ([(for-node loopvar start end incr body) node])
             (append (list loopvar start end incr) body)))
         (define (node-map fn node)
           (map (lambda (n) (node-map fn n)) (node-children node)))
         (define (node-dependencies node)
           (for/list ([child (for-node-body node)])
                     (filter-before (lambda (c) (dependent? child c))
                                    (lambda (c) (eq? c child))
                                    (for-node-body node))))
        [(define (node-children node)
           (match-let ([(for-node loopvar start end incr body pragmas) node])
             (append (list loopvar start end incr) body)))
         (define (node-accesses node)
           (let ([children (node-children node)])
             (collect-uniq (map super-accesses children))))])

(struct assign
        (target value)
        #:transparent
        #:methods gen:node
        [(define/generic super-accesses node-accesses)
         (define (node-children node)
           (match-let ([(assign target value) node])
             (list target value)))
         (define (node-accesses node)
           (match-let* ([(assign target value) node]
                        [(list reads writes) (super-accesses value)])
             (set-add! writes target)
             (list reads writes)))])

(struct binop
        (op1 op2)
        #:transparent
        #:methods gen:node
        [(define/generic super-accesses node-accesses)
         (define (node-children node)
           (match-let ([(binop op1 op2) node])
             (list op1 op2)))
         (define (node-accesses node)
           (let ([children (node-children node)])
             (collect-uniq (map super-accesses children))))])

(struct add binop () #:transparent)

(struct mul binop () #:transparent)

(struct lt binop  () #:transparent)

(struct array-reference
        (arr index)
        #:transparent
        #:methods gen:node
        [(define/generic super-accesses node-accesses)
         (define (node-children node)
           (match-let ([(array-reference arr index) node])
             (list arr index)))
         (define (node-accesses node)
           (match-let* ([(array-reference arr index) node]
                        [children (node-children node)]
                        [(list reads writes) (collect-uniq (map super-accesses children))])
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
           (list (symbol-name node)))
         (define (node-accesses node)
           (list (mutable-set (symbol-name node)) (mutable-set)))])

(struct func-decl
        (ret-type name params body)
        #:transparent
        #:methods gen:node
        [(define/generic super-accesses node-accesses)
         (define (node-dependencies node)
          (for/list ([child (block-stmts (func-decl-body node))])
                    (filter-before (lambda (c) (dependent? child c))
                                   (lambda (c) (eq? c child))
                                   (block-stmts (func-decl-body node)))))])
(struct return    (target)                         #:transparent)
(struct block     (stmts return)                   #:transparent)
(struct param     (name type)                      #:transparent)
(struct allocate  (target type rows cols)          #:transparent)


(define int-ptr 0)
(define mat int-ptr)
(define int 1)
(define pragma-ignore-loop-deps 2)

(define (for-loop loopvar start end incr body [pragmas '()])
  (for-node
   (if (symbol? loopvar) loopvar (symbol loopvar))
   (num start)
   (num end)
   (num incr)
   body
   pragmas))

;; A for loop wrapped in a list, basically a block without any return statementss
(define (for-block loopvar start end incr body [pragmas '()])
  (list (for-loop loopvar start end incr body pragmas)))
