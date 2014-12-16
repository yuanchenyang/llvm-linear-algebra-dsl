#lang racket

(require racket/pretty)
(require racket-llvm/unsafe)

(require fast-math/nodes)
(require fast-math/utils)
(require fast-math/matrix)
(provide (all-defined-out))

(define context (LLVMContextCreate))
(define int-type (LLVMInt32TypeInContext context))
(define float-type (LLVMDoubleTypeInContext context))
(define bool-type (LLVMInt1TypeInContext context))

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (create-entry-block-alloca func varname)
  (let ([tmp-builder (LLVMCreateBuilder)]
        [block (LLVMGetEntryBasicBlock func)])
    (LLVMPositionBuilder tmp-builder block (LLVMGetFirstInstruction block))
    (LLVMBuildAlloca tmp-builder int-type varname)))

(define (compile-assign-symbol node builder env context value)
  (let* ([target-name (symbol-name (assign-target node))]
         [var (hash-ref env target-name null)])
    (if (not (null? var)) (LLVMBuildStore builder value var)
        (let ([alloca (LLVMBuildAlloca builder (LLVMTypeOf value) target-name)])
          (hash-set! env target-name alloca)
          (LLVMBuildStore builder value alloca)))))

(define (compile-assign-array-ref node builder env context value)
  (match-let* ([(array-reference arr index) (assign-target node)]
               [var (hash-ref env (symbol-name arr)
                              (lambda () (error "Assigning to undeclared array")))]
               [index (compile-ast-to-llvm index builder env context)]
               [gep (LLVMBuildGEP builder var (list index) (gen-unique-name))])
	      (LLVMBuildStore builder value gep)))

(define (compile-assign node builder env context)
  (let ([value (compile-ast-to-llvm (assign-value node) builder env context)])
    (if (symbol? (assign-target node))
      (compile-assign-symbol node builder env context value)
      (compile-assign-array-ref node builder env context value))))

(define (compile-start-val node builder env context alloca) 
  (let ([start-val (compile-ast-to-llvm node builder env context)]) 
    (LLVMBuildStore builder start-val alloca)))

(define (compile-update-loop-var incr builder env context alloca loop-var-name)
  (let* ([step-val (compile-ast-to-llvm incr builder env context)]
         [curr-var (LLVMBuildLoad builder alloca loop-var-name)]
         [next-var (LLVMBuildAdd builder curr-var step-val "next-var")])
    (LLVMBuildStore builder next-var alloca)))

(define (compile-for-node node builder env context)
  (let* ([loop-var-name (symbol-name (for-node-loop-var node))]
         [alloca (create-entry-block-alloca
                  (builder->function builder) loop-var-name)]
         [loop-block-name (string-append "loop" (number->string (gen-unique-num)))]
         [loop-block (LLVMAppendBasicBlockInContext
                      context (builder->function builder) loop-block-name)]
         [old-val (hash-ref env loop-var-name null)])
    (compile-start-val (for-node-init node) builder env context alloca)
    (LLVMBuildBr builder loop-block)
    (LLVMPositionBuilderAtEnd builder loop-block)
    (hash-set! env loop-var-name alloca)
    (map (lambda (node) (compile-ast-to-llvm node builder env context))
         (for-node-body node))
    (compile-update-loop-var (for-node-incr node) builder env context alloca 
                             loop-var-name)
    (let* ([end (compile-ast-to-llvm 
                 (lt (symbol loop-var-name) 
                     (for-node-end node)) builder env context)]
           [after-block (LLVMAppendBasicBlockInContext
                         context
                         (builder->function builder)
                         (string-append "after" loop-block-name))])
      (LLVMBuildCondBr builder end loop-block after-block)
      (LLVMPositionBuilderAtEnd builder after-block))
    (if (not (empty? old-val))
        (hash-set! env loop-var-name old-val)
        '())))

(define (compile-return node builder env context ret-type)
  (let* ([target (return-target node)]
	 [retval (if (and (symbol? target) (= ret-type int-ptr))
		     (hash-ref env (symbol-name target) '())
		     (compile-ast-to-llvm target builder env context))])
    (LLVMBuildRet builder retval)
    (if (and (symbol? target) (= ret-type int-ptr))
	(symbol-name target)
	null)))

(define (compile-binop node builder env context)
  (let* ([op1 (compile-ast-to-llvm (binop-op1 node) builder env context)]
         [op2 (compile-ast-to-llvm (binop-op2 node) builder env context)]
         [op1-type (LLVMTypeOf op1)]
         [operator (if (equal? op1-type float-type)
                       (cond [(add? node) LLVMBuildFAdd]
                             [(mul? node) LLVMBuildFMul]
                             [(sub? node) LLVMBuildFSub]
                             [(div? node) LLVMBuildFDiv]
                             [else (error "Unsupported binop")])
                       (cond [(add? node) LLVMBuildAdd]
                             [(mul? node) LLVMBuildMul]
                             [(sub? node) LLVMBuildSub]
                             [(div? node) LLVMBuildSDiv]
                             [else (error "Unsupported binop")]))])
    (operator builder op1 op2 (gen-unique-name))))

(define (compile-pred node builder env context operator)
  (let ([op1 (compile-ast-to-llvm (binop-op1 node) builder env context)]
        [op2 (compile-ast-to-llvm (binop-op2 node) builder env context)])
    (LLVMBuildICmp builder operator op1 op2 (gen-unique-name))))

(define (compile-num node context)
  (let ([value (num-value node)])
    (if (flonum? value)
        (LLVMConstReal float-type value)
        (LLVMConstInt int-type value #t))))

(define (compile-array-ref node builder env context)
  (match-let ([(array-reference arr index) node]
              [name (gen-unique-name)])
    (define ptr (LLVMBuildGEP builder (hash-ref env (symbol-name arr))
                              (list (compile-ast-to-llvm index builder env context)) name))
    (LLVMBuildLoad builder ptr name)))

(define (compile-symbol node builder env context)
  (LLVMBuildLoad builder (hash-ref env (symbol-name node)) (symbol-name node)))

(define (compile-ast-to-llvm node builder env context)
  (cond [(return? node)          (compile-return    node builder env context)]
        [(lt? node)              (compile-pred      node builder env context 'LLVMIntSLT)]
        [(binop? node)           (compile-binop     node builder env context)]
        [(for-node? node)        (compile-for-node  node builder env context)]
        [(assign? node)          (compile-assign    node builder env context)]
        [(num? node)             (compile-num       node context)]
        [(array-reference? node) (compile-array-ref node builder env context)]
        [(symbol? node)          (compile-symbol    node builder env context)]
        [else (error "Unsupported node")]))

(define (process-int builder func param index)
  (let ([x (LLVMGetParam func index)] 
        [alloca (create-entry-block-alloca func (gen-unique-name))]
        [name (param-name param)])
    (LLVMSetValueName x name)
    (LLVMBuildStore builder x alloca)
    (cons name alloca)))

(define (process-matrix builder func param index)
  (let* ([x (LLVMGetParam func index)]
         [name (param-name param)])
    (LLVMSetValueName x name)
    (cons name x)))

(define (get-type param)
  (cond [(= int-ptr (param-type param)) (LLVMPointerType float-type 0)]
        [(= int (param-type param)) int-type]
        [else (error "Unsupported arg type")]))

(define (convert-type type)
  (cond [(= int-ptr type) (LLVMPointerType float-type 0)]
	[(= int type) int-type]))

(define (split pred list)
  (if (null? list) (cons '() '())
      (let* ([curr (car list)]
	     [rest (split pred (cdr list))]
	     [trues (car rest)]
	     [falses (cdr rest)])
        (if (pred curr)
	    (cons (cons curr trues) falses)
	    (cons trues (cons curr falses))))))

(define (convert-arg arg) 
  (cond [(matrix? arg) (LLVMCreateGenericValueOfPointer (matrix-contents arg))]
        [(integer? arg) (LLVMCreateGenericValueOfInt int-type arg #t)]
        [else (error "Unsupport argument type")]))

(define (get-params-from-allocs alloc)
  (param (symbol-name (allocate-target alloc))
         (allocate-type alloc)))

(define (build-symbol-table params builder fun)
  (for/list ([index (in-naturals 0)] [param params]) 
    (let ([type (param-type param)])
      (cond [(= int-ptr type) (process-matrix builder fun param index)]
            [(= int type)     (process-int    builder fun param index)]
            [else             (error "Unsupport argument type")]))))

(define (build-matrix-from-alloc alloc)
  (make-zero-matrix (symbol-name (allocate-target alloc))
                    (allocate-rows alloc) (allocate-cols alloc)))

(define (get-return-matrix output args-and-allocs ret-symb)
  (let ([ptr (LLVMGenericValueToPointer output)]
        [mat (car (filter (lambda (x) (equal? (matrix-id x) ret-symb)) args-and-allocs))])
    (make-matrix-with-ptr (matrix-id mat) (matrix-rows mat)
                          (matrix-cols mat) ptr)))

(define (do-math program)
  (begin
    (pretty-print program)
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
    (define allocs-body (split allocate? (block-stmts (func-decl-body program))))
    (define allocs (car allocs-body))
    (define body (cdr allocs-body))
    (define params (append (func-decl-params program) (map get-params-from-allocs allocs)))
    (define param-types (map get-type params))
    (define fun-type (LLVMFunctionType
                      (convert-type (func-decl-ret-type program)) param-types false))
    (define fun (LLVMAddFunction module (func-decl-name program) fun-type))
    (let ()
      (define entry (LLVMAppendBasicBlockInContext context fun "entry"))
      (define builder (LLVMCreateBuilderInContext context))

      (LLVMPositionBuilderAtEnd builder entry)
      (define env (make-hash (build-symbol-table params builder fun)))

      (map (lambda (statement)
             (compile-ast-to-llvm statement builder env context))
           body)
      (define ret-symb (compile-return (block-return (func-decl-body program))
				       builder env context (func-decl-ret-type program)))
      (LLVMDumpModule module)
      ;; (define mem2reg (LLVMCreatePromoteMemoryToRegisterPass))
      (LLVMOptimizeModule module)
      (LLVMDumpModule module)

      (let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
        (when err
          (display err) (exit 1)))
      (lambda args
        (define args-and-allocs (append args (map build-matrix-from-alloc allocs))) 
        (define processed-args (map convert-arg args-and-allocs))
        (LLVMLinkInJIT)
        (define ee (LLVMCreateExecutionEngineForModule module))
        (define output (LLVMRunFunction ee fun processed-args))
        (cond [(= (func-decl-ret-type program) int-ptr)
               (get-return-matrix output args-and-allocs ret-symb)]
              [(= (func-decl-ret-type program) int) (LLVMGenericValueToInt output #t)])))))
