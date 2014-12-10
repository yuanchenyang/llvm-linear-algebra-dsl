#lang racket

(provide for
	 assign
	 add
	 array-ref
	 num
	 symbol)

(struct for (loop-var init end incr body))
(struct assign (target value))
(struct add (op1 op2))
(struct array-ref (arr index))
(struct num (value))
(struct symbol (name))
