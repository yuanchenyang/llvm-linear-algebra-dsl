#lang racket

(provide (all-defined-out))

(struct for-node   (loop-var init end incr body) #:transparent)
(struct assign     (target value)                #:transparent)
(struct add        (op1 op2)                     #:transparent)
(struct mul        (op1 op2)                     #:transparent)
(struct array-ref  (arr index)                   #:transparent)
(struct num        (value)                       #:transparent)
(struct symbol     (name)                        #:transparent)
(struct func-decl  (name args body)              #:transparent)
(struct return     (target)                      #:transparent)
(struct block      (stmts return)                #:transparent)

(provide (all-defined-out))
>>>>>>> ca6071c3e54f57c1393187acf387cda9a2622b3a
