#lang racket
(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define i (symbol "i"))
(define j (symbol "j"))

(define tree
  (func-decl
   (symbol "matrix-add")
   (list a b c)
   (list
    (for i (num 0) (num 10) (num 1)
	 (for j (num 0) (num 10) (num 1)
	      (assign (array-ref c (add (mul i (num 10)) j))
		      (add (array-ref a (add (mul i (num 10)) j))
			   (array-ref b (add (mul i (num 10)) j)))))))))

(define (do-math program)
  (begin
    (define context (LLVMContextCreate))
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
    (define int-type (LLVMInt32TypeInContext context))
    (pretty-print program)))

(do-math tree)
