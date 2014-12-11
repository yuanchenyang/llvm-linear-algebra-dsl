#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)
(require "utils.rkt")

(define (compile-for-node node builder env context)
  '())

(define (compile-return node builder env context)
  (LLVMBuildRet
   builder
   (compile-ast-to-llvm (return-target node) builder env context)))

(define (compile-add node builder env context)
  (let ([op1 (compile-ast-to-llvm (add-op1 node) builder env context)]
	[op2 (compile-ast-to-llvm (add-op2 node) builder env context)])
    (LLVMBuildAdd builder op1 op2 (gen-unique-symbol))))

(define (compile-ast-to-llvm node builder env context)
  (cond [(return? node)   (compile-return   node builder env context)]
	[(add? node)      (compile-add      node builder env context)]
	[(for-node? node) (compile-for-node node builder env context)]
	[(symbol? node)   (hash-ref env (symbol-name node))]
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

      (compile-ast-to-llvm (func-decl-body program) builder env context)
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

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define i (symbol "i"))
(define j (symbol "j"))

(define array-add
  (func-decl
   "matrix-add"
   (list a b c)
   (list
    (for-node i (num 0) (num 10) (num 1)
	 (for-node j (num 0) (num 10) (num 1)
	      (assign (array-reference c (add (mul i (num 10)) j))
		      (add (array-reference a (add (mul i (num 10)) j))
			   (array-reference b (add (mul i (num 10)) j)))))))))

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

(test-begin
   "Test simple add"
   (check-eq? (do-math add-func (list 5 2)) 7))

