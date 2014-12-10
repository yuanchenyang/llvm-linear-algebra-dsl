#lang racket

(struct for-node   (loop-var init end incr body) #:transparent)
(struct assign     (target value)                #:transparent)
(struct add        (op1 op2)                     #:transparent)
(struct mul        (op1 op2)                     #:transparent)
(struct array-ref  (arr index)                   #:transparent)
(struct num        (value)                       #:transparent)
(struct symbol     (name)                        #:transparent)
(struct func-decl  (name args body)              #:transparent)
(struct block      (stmts return)                #:transparent)

(provide (all-defined-out))
