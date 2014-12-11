#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)
(require "utils.rkt")


(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))


(define (compile-assign node builder env context)
  (let ([value (compile-ast-to-llvm (assign-value node) builder env context)]
	[target (compile-ast-to-llvm (assign-target node) builder env context)])
    (LLVMBuildStore builder value target)))

(define (compile-for-node node builder env context)
  (begin
    ;; Create a new block
    (define block (LLVMAppendBasicBlockInContext
		   context (builder->function builder) "loop"))
    ;; Hardcoding int-type
    (define int-type (LLVMInt32TypeInContext context))
    ;; Allocate loop variable
    (define loop-var-name (symbol-name (for-node-loop-var node)))
    (define loop-var (LLVMBuildAlloca builder int-type loop-var-name))
    (define old-val (hash-ref env loop-var '()))
    (hash-set! env loop-var-name loop-var)
    (compile-ast-to-llvm (assign (for-node-loop-var node) (for-node-init node))
    			 builder env context)
    (LLVMBuildBr builder block)
    (LLVMPositionBuilderAtEnd builder block)
    (compile-ast-to-llvm (for-node-body node) builder env context)
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
    (define int-type (LLVMInt32TypeInContext context))
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

      (compile-ast-to-llvm (func-decl-body program) builder env context)
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
	     (return (add (symbol "x") (symbol "y")))))

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define i (symbol "i"))
(define j (symbol "j"))

(define array-add
  (func-decl
   "matrix-add"
   (list a b c)
   (for-node i (num 0) (num 10) (num 1)
	     (for-node j (num 0) (num 10) (num 1)
		       (assign (array-reference c (add (mul i (num 10)) j))
			       (add (array-reference a (add (mul i (num 10)) j))
				    (array-reference b (add (mul i (num 10)) j))))))))

(require ffi/unsafe
	 racket/flonum)

(define A
  (let ([v (make-flvector 100)])
    (begin0 v
      (for ([x (in-range 100)])
	(flvector-set! v x 1.0)))))

(define B
  (let ([v (make-flvector 100)])
    (begin0 v
      (for ([x (in-range 100)])
	(flvector-set! v x 1.0)))))

(define C
  (let ([v (make-flvector 100)])
    (begin0 v
      (for ([x (in-range 100)])
	(flvector-set! v x 0.0)))))

(LLVMCreateGenericValueOfPointer (flvector->cpointer A))

(do-math array-add (list A B C))

(test-begin
   "Test simple add"
   (check-eq? (do-math add-func (list 5 2)) 7))

