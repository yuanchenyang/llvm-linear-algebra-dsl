#lang racket

(provide (all-defined-out))

(struct for-node        (loop-var init end incr body) #:transparent)
(struct assign          (target value)                #:transparent)
(struct binop           (op1 op2)                     #:transparent)
(struct add binop       ()                            #:transparent)
(struct mul binop       ()                            #:transparent)
(struct lt binop        ()                            #:transparent)
(struct array-reference (arr index)                   #:transparent)
(struct num             (value)                       #:transparent)
(struct symbol          (name)                        #:transparent)
(struct func-decl       (ret-type name params body)   #:transparent)
(struct return          (target)                      #:transparent)
(struct block           (stmts return)                #:transparent)
(struct param           (name type)                   #:transparent)
(struct allocate        (target type rows cols)       #:transparent)

(define int-ptr 0)
(define int 1)

(define (for-loop loopvar start end incr body)
  (for-node (if (symbol? loopvar) loopvar (symbol loopvar))
            (num start)
            (num end)
            (num incr)
            body))
