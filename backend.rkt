
#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)
(require "utils.rkt")

(define context (LLVMContextCreate))
(define int-type (LLVMInt32TypeInContext context))

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (create-entry-block-alloca func varname)
  (let ([tmp-builder (LLVMCreateBuilder)])
    (LLVMPositionBuilderAtEnd tmp-builder (LLVMGetEntryBasicBlock func))
    (LLVMBuildAlloca tmp-builder int-type varname)))

(define (compile-assign node builder env context)
  (let* ([target-name (symbol-name (assign-target node))]
	 [entry-bb (builder->function builder)]
	 [a (create-entry-block-alloca entry-bb target-name)])
    ;; TODO: Handle name shadowing, we have to keep old bindings and push that back on later
    (hash-set! env target-name a)
    (LLVMBuildStore builder (compile-ast-to-llvm (assign-value node) builder env context) a)))

(define (compile-for-node node builder env context)
  (begin
    (define init-var (gen-unique-symbol))
    (define loop-var-name (symbol-name (for-node-loop-var node)))
    (define alloca (create-entry-block-alloca (builder->function builder) loop-var-name))
    (define start-val (compile-ast-to-llvm (for-node-init node) builder env context))
    (LLVMBuildStore builder start-val alloca)
    ;; Create a new block
    (define loop-block (LLVMAppendBasicBlockInContext
			context
			(builder->function builder)
			(string-append "loop" (number->string (gen-unique-num)))))
    (LLVMBuildBr builder loop-block)
    (LLVMPositionBuilderAtEnd builder loop-block)
    (define old-val (hash-ref env loop-var-name '()))
    (hash-set! env loop-var-name alloca)
    (map (lambda (node) (compile-ast-to-llvm node builder env context))
	 (for-node-body node))
    (define step-val (compile-ast-to-llvm (for-node-incr node) builder env context))
    (define end (compile-ast-to-llvm (lt (for-node-loop-var node)
					 (for-node-end node)) builder env context))
    (define curr-var (LLVMBuildLoad builder alloca loop-var-name))
    (define next-var (LLVMBuildAdd builder curr-var step-val "next-var"))
    (LLVMBuildStore builder next-var alloca)
    (define after-block (LLVMAppendBasicBlockInContext
			context
			(builder->function builder)
			(string-append "afterloop" (number->string (gen-unique-num)))))
    (LLVMBuildCondBr builder end loop-block after-block)
    (LLVMPositionBuilderAtEnd builder after-block)
    (if (not (null? old-val))
	(hash-set! env loop-var-name old-val)
	'())))

(define (compile-return node builder env context)
  (LLVMBuildRet
   builder
   (compile-ast-to-llvm (return-target node) builder env context)))

(define (compile-binop node builder env context operator)
  (let ([op1 (compile-ast-to-llvm (binop-op1 node) builder env context)]
	[op2 (compile-ast-to-llvm (binop-op2 node) builder env context)])
    (operator builder op1 op2 (gen-unique-symbol))))

(define (compile-pred node builder env context operator)
  (let ([op1 (compile-ast-to-llvm (binop-op1 node) builder env context)]
	[op2 (compile-ast-to-llvm (binop-op2 node) builder env context)])
    (operator builder 'LLVMIntULT op1 op2 (gen-unique-symbol))))


(define (compile-num node context)
  (LLVMConstInt int-type (num-value node) #t))

(define (compile-array-ref node builder env context)
  (error "compile-array-ref not implemented"))

(define (compile-symbol node builder env context)
   (LLVMBuildLoad builder (hash-ref env (symbol-name node)) (symbol-name node)))

(define (compile-ast-to-llvm node builder env context)
  (cond [(return? node)          (compile-return    node builder env context)]
	[(add? node)             (compile-binop     node builder env context LLVMBuildAdd)]
	[(lt? node)              (compile-pred      node builder env context LLVMBuildICmp)]
	[(for-node? node)        (compile-for-node  node builder env context)]
	[(assign? node)          (compile-assign    node builder env context)]
	[(num? node)             (compile-num       node context)]
	[(array-reference? node) (compile-array-ref node builder env context)]
	[(symbol? node)          (compile-symbol    node builder env context)]
	[else (error "Unsupported node")]))

(define (process-params builder func params index)
  (if (null? params) '()
      (let ([x (LLVMGetParam func index)]
	    [param (symbol-name (car params))]
	    [alloca (create-entry-block-alloca func (gen-unique-symbol))])
	(LLVMSetValueName x param)
	(LLVMBuildStore builder x alloca)
	(cons (cons param alloca)
	      (process-params builder func (cdr params) (+ index 1))))))

(define (do-math program args)
  (begin
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
    (define param-types (map (lambda (a) int-type) (func-decl-params program)))
    (define fun-type (LLVMFunctionType int-type param-types false))
    (define fun (LLVMAddFunction module (func-decl-name program) fun-type))
    (let ()
      (define entry (LLVMAppendBasicBlockInContext context fun "entry"))
      (define builder (LLVMCreateBuilderInContext context))

      (LLVMPositionBuilderAtEnd builder entry)
      (define env
	(make-hash
	 (process-params builder fun (func-decl-params program) 0)))

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

