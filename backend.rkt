#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)
(require "utils.rkt")

(define context (LLVMContextCreate))
(define int-type (LLVMInt32TypeInContext context))

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))


(define (compile-assign node builder env context)
  (begin
    (define value (compile-ast-to-llvm (assign-value node) builder env context))
    (define target-name (symbol-name (assign-target node)))
    (hash-set! env target-name (LLVMBuildAlloca builder int-type target-name))
    (define target (compile-ast-to-llvm (assign-target node) builder env context))
    (LLVMBuildStore builder value target)))

(define (compile-for-node node builder env context)
  (begin
    ;; Create a new block
    (define block (LLVMAppendBasicBlockInContext
		   context (builder->function builder) (string-append "loop" (gen-unique-symbol))))
    ;; Allocate loop variable
    (define loop-var-name (symbol-name (for-node-loop-var node)))
    (define old-val (hash-ref env loop-var-name '()))
    ;; (define loop-var (LLVMBuildAlloca builder int-type loop-var-name))
    ;; (hash-set! env loop-var-name loop-var)
    (compile-ast-to-llvm (assign (for-node-loop-var node) (for-node-init node))
    			 builder env context)
    (LLVMBuildBr builder block)
    (LLVMPositionBuilderAtEnd builder block)
    (compile-ast-to-llvm (car (for-node-body node)) builder env context)
    (hash-remove! env loop-var-name)
    ))

(define (compile-return node builder env context)
  (LLVMBuildRet
   builder
   (compile-ast-to-llvm (return-target node) builder env context)))

(define (compile-add node builder env context)
  (let ([op1 (compile-ast-to-llvm (add-op1 node) builder env context)]
	[op2 (compile-ast-to-llvm (add-op2 node) builder env context)])
    (LLVMBuildAdd builder op1 op2 (gen-unique-symbol))))

(define (compile-num node context)
  (begin
    (LLVMConstInt int-type (num-value node) #t)))

(define (compile-array-ref node builder env context)
  (error "compile-array-ref not implemented"))

(define (compile-ast-to-llvm node builder env context)
  (cond [(return? node)          (compile-return    node builder env context)]
	[(add? node)             (compile-add       node builder env context)]
	[(for-node? node)        (compile-for-node  node builder env context)]
	[(assign? node)          (compile-assign    node builder env context)]
	[(num? node)             (compile-num       node context)]
	[(array-reference? node) (compile-array-ref node builder env context)]
	[(symbol? node)          (hash-ref env (symbol-name node))]
	[else (error "Unsupported node")]))

(define (process-params func params index)
  (if (null? params) '()
      (let ([x (LLVMGetParam func index)]
	    [param (symbol-name (car params))])
	(LLVMSetValueName x param)
	(cons (cons param x)
	      (process-params func (cdr params) (+ index 1))))))

(define (do-math program args)
  (begin
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
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

      (map (lambda (statement)
	     (compile-ast-to-llvm statement builder env context))
	   (func-decl-body program))
      (LLVMDumpModule module)

      (let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
       (when err
         (display err) (exit 1)))
      (define int-args (map (lambda (arg)
			      (LLVMCreateGenericValueOfInt int-type arg)) args))
      (LLVMLinkInJIT)
      (define ee (LLVMCreateExecutionEngineForModule module))
      
      (define output (LLVMRunFunction ee fun int-args))
      (LLVMGenericValueToInt output #t)
    )))


(require rackunit)

(define add-func
  (func-decl "add" (list (symbol "x") (symbol "y"))
  	  (list (return (add (symbol "x") (symbol "y"))))))

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define i (symbol "i"))
(define j (symbol "j"))

(define loop-add
  (func-decl
   "matrix-add"
   (list a c)
   (list
    (for-node i (num 0) (num 10) (num 1)
	      (list (for-node j (num 0) (num 10) (num 1)
			      (list (add c a))))))))

(require ffi/unsafe
	 racket/flonum)

(do-math loop-add (list 10 10))

(test-begin
   "Test simple add"
   (check-eq? (do-math add-func (list 5 2)) 7))

