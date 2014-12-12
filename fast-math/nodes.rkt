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
(struct func-decl       (name params body)            #:transparent)
(struct return          (target)                      #:transparent)
(struct block           (stmts return)                #:transparent)
(struct param           (name type)                   #:transparent)

(define int-ptr 0)
(define int 1)