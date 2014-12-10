#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define i (symbol "i"))
(define j (symbol "j"))

(define add-func
  (return (add (symbol "x") (symbol "y"))))

(define (compile-ast-to-llvm node builder env)
  (cond [(return? node) (LLVMBuildRet builder (compile-ast-to-llvm
					       (return-target node) builder env))]
	[(add? node) (let ([op1 (compile-ast-to-llvm (add-op1 node) builder env)]
			   [op2 (compile-ast-to-llvm (add-op2 node) builder env)])
		       (LLVMBuildAdd builder op1 op2 "a"))]
	[(symbol? node) (hash-ref env (symbol-name node))]
	[else (error "Unsupport node")]))

(define (do-math program)
  (begin
    (define context (LLVMContextCreate))
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
    (define int-type (LLVMInt32TypeInContext context))
    (define fun-type (LLVMFunctionType int-type (list int-type int-type) false))
    (define add-fun (LLVMAddFunction module "add" fun-type))
    (let ()
      (define x (LLVMGetParam add-fun 0))
      (define y (LLVMGetParam add-fun 1))
      (LLVMSetValueName x "x")
      (LLVMSetValueName y "y")
      (define env
	(make-hash
	 (list (cons "x" x)
               (cons "y" x))))
      (define entry (LLVMAppendBasicBlockInContext context add-fun "entry"))
      (define builder (LLVMCreateBuilderInContext context))

      (LLVMPositionBuilderAtEnd builder entry)
      (compile-ast-to-llvm program builder env)
      (LLVMDumpModule module)
    )))

(do-math add-func)

;; (define tree
;;   (func-decl
;;    (symbol "matrix-add")
;;    (list a b c)
;;    (list
;;     (for i (num 0) (num 10) (num 1)
;; 	 (for j (num 0) (num 10) (num 1)
;; 	      (assign (array-ref c (add (mul i (num 10)) j))
;; 		      (add (array-ref a (add (mul i (num 10)) j))
;; 			   (array-ref b (add (mul i (num 10)) j)))))))))
