#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)
(require "utils.rkt")


(define (compile-ast-to-llvm node builder env)
  (cond [(return? node) (LLVMBuildRet builder (compile-ast-to-llvm
					       (return-target node) builder env))]
	[(add? node) (let ([op1 (compile-ast-to-llvm (add-op1 node) builder env)]
			   [op2 (compile-ast-to-llvm (add-op2 node) builder env)])
		       (LLVMBuildAdd builder op1 op2 (gen-unique-symbol)))]
	[(symbol? node) (hash-ref env (symbol-name node))]
	[else (error "Unsupport node")]))

(define (process-params func params index)
  (if (null? params) '()
      (let ([x (LLVMGetParam func index)]
	    [param (symbol-name (car params))])
	(LLVMSetValueName x param)
	(cons (cons param x)
	      (process-params func (cdr params) (+ index 1))))))

(define (do-math program args)
  (begin
    (define context (LLVMContextCreate))
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
    (define int-type (LLVMInt32TypeInContext context))
    (define param-types (map (lambda (a) int-type) (func-decl-params program)))
    (define fun-type (LLVMFunctionType int-type param-types false))
    (define fun (LLVMAddFunction module (func-decl-name program) fun-type))
    (let ()
      (define env
	(make-hash
	 (process-params fun (func-decl-params program) 0)))
      (define entry (LLVMAppendBasicBlockInContext context fun "entry"))
      (define builder (LLVMCreateBuilderInContext context))

      (LLVMPositionBuilderAtEnd builder entry)

      (compile-ast-to-llvm (func-decl-body program) builder env)
      (LLVMDumpModule module)

      (let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
       (when err
         (display err) (exit 1)))
      (define arg1 (LLVMCreateGenericValueOfInt int-type (car args) #t))
      (define arg2 (LLVMCreateGenericValueOfInt int-type (car (cdr args)) #t))
      (LLVMLinkInJIT)
      (define ee (LLVMCreateExecutionEngineForModule module))
      
      (define output (LLVMRunFunction ee fun (list arg1 arg2)))
      (LLVMGenericValueToInt output #t)
    )))


(require rackunit)

(define add-func
  (func-decl "add" (list (symbol "x") (symbol "y"))
	     (return (add (symbol "x") (symbol "y")))))

(test-begin
   "Test simple add"
   (check-eq? (do-math add-func (list 5 2)) 7))

