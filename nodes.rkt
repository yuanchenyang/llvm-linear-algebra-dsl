#lang racket

(provide (all-defined-out))

(struct for        (loop-var init end incr body) #:transparent)
(struct assign     (target value)                #:transparent)
(struct add        (op1 op2)                     #:transparent)
(struct mul        (op1 op2)                     #:transparent)
(struct array-ref  (arr index)                   #:transparent)
(struct num        (value)                       #:transparent)
(struct symbol     (name)                        #:transparent)
(struct func-decl  (name args body)              #:transparent)
(struct return     (target)                      #:transparent)
