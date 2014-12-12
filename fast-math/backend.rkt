#lang racket

(require "nodes.rkt")
(require racket/pretty)
(require racket-llvm/unsafe)
(require "utils.rkt")
(require "frontend.rkt")
(provide (all-defined-out))

(define context (LLVMContextCreate))
(define int-type (LLVMInt32TypeInContext context))
(define bool-type (LLVMInt1TypeInContext context))

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (create-entry-block-alloca func varname)
  (let ([tmp-builder (LLVMCreateBuilder)])
    (LLVMPositionBuilderAtEnd tmp-builder (LLVMGetEntryBasicBlock func))
    (LLVMBuildAlloca tmp-builder int-type varname)))

(define (compile-assign-symbol node builder env context value)
  (let* ([target-name (symbol-name (assign-target node))]
         [var (hash-ref! env target-name '())])
    (if (not (empty? var)) (LLVMBuildStore builder value var)
        (let ([alloca (LLVMBuildAlloca builder int-type target-name)])
          (hash-set! env target-name alloca)
          (LLVMBuildStore builder value alloca)))))

(define (compile-assign-array-ref node builder env context value)
  (match-let* ([(array-reference arr index) (assign-target node)]
               [var (hash-ref! env arr '())]
               [index (compile-ast-to-llvm index builder env context)]
               [gep (LLVMBuildGEP builder var (list index) (gen-unique-symbol))])
             (if (null? var) (error "Assigning to undeclared array")
               (LLVMBuildStore builder value gep))))

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
         [alloca (create-entry-block-alloca (builder->function builder) 
                                            loop-var-name)]
         [loop-block-name (string-append "loop" (number->string (gen-unique-num)))]
         [loop-block (LLVMAppendBasicBlockInContext context
                        (builder->function builder) loop-block-name)]
         [insert-block (LLVMGetInsertBlock builder)]
         [old-val (hash-ref env loop-var-name null)]
         )
    (compile-start-val (for-node-init node) builder env context alloca)
    ;; Create a new block
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
      (LLVMPositionBuilderAtEnd builder insert-block)
      (LLVMBuildBr builder loop-block)
      (LLVMPositionBuilderAtEnd builder after-block))
    (if (not (empty? old-val))
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
    (LLVMBuildICmp builder operator op1 op2 (gen-unique-symbol))))


(define (compile-num node context)
  (LLVMConstInt int-type (num-value node) #f))

(define (compile-array-ref node builder env context)
  (match-let ([(array-reference arr index) node]
              [name (gen-unique-symbol)])
    (define ptr (LLVMBuildGEP builder (hash-ref! env arr '())
                  (list (compile-ast-to-llvm index builder env context)) name))
    (LLVMBuildLoad builder ptr name)))

(define (compile-symbol node builder env context)
   (LLVMBuildLoad builder (hash-ref env (symbol-name node)) (symbol-name node)))

(define (compile-ast-to-llvm node builder env context)
  (cond [(return? node)          (compile-return    node builder env context)]
        [(add? node)             (compile-binop     node builder env context LLVMBuildAdd)]
        [(mul? node)             (compile-binop     node builder env context LLVMBuildMul)]
        [(lt? node)              (compile-pred      node builder env context 'LLVMIntULT)]
        [(for-node? node)        (compile-for-node  node builder env context)]
        [(assign? node)          (compile-assign    node builder env context)]
        [(num? node)             (compile-num       node context)]
        [(array-reference? node) (compile-array-ref node builder env context)]
        [(symbol? node)          (compile-symbol    node builder env context)]
        [else (error "Unsupported node")]))

(define (process-int builder func param index)
  (let ([x (LLVMGetParam func index)] 
        [alloca (create-entry-block-alloca func (gen-unique-symbol))]
        [name (param-name param)])
    (LLVMSetValueName x name)
    (LLVMBuildStore builder x alloca)
    (cons name alloca)))

(define (process-matrix builder func param index)
  (let* ([x (LLVMGetParam func index)]
         [name (param-name param)])
    (LLVMSetValueName x name)
    (cons name x)))

(define (get-type arg)
  (cond [(matrix? arg) (LLVMPointerType int-type 0)]
        [(integer? arg) int-type]
        [else (error "Unsupported arg type")]))

(define (do-math program args)
  (begin
    (define module (LLVMModuleCreateWithNameInContext "jit-module" context))
    (define param-types (map get-type args))
    (define fun-type (LLVMFunctionType int-type param-types false))
    (define fun (LLVMAddFunction module (func-decl-name program) fun-type))
    (let ()
      (define entry (LLVMAppendBasicBlockInContext context fun "entry"))
      (define builder (LLVMCreateBuilderInContext context))

      (LLVMPositionBuilderAtEnd builder entry)
      (define env (make-hash
        (for/list ([arg args]
                   [index (in-range (length args))]
                   [param (func-decl-params program)]) 
                  (cond [(matrix? arg) (process-matrix builder fun param index)] 
                        [(integer? arg) (process-int builder fun param index)]
                        [else (error "Unsupport argument type")]))))

      (map (lambda (statement)
             (compile-ast-to-llvm statement builder env context))
           (func-decl-body program))
      (LLVMDumpModule module)

      (let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
       (when err
         (display err) (exit 1)))
      (define (convert-arg arg) 
        (cond [(matrix? arg) (LLVMCreateGenericValueOfPointer (matrix-contents arg))]
              [(integer? arg) (LLVMCreateGenericValueOfInt int-type arg #t)]
              [else (error "Unsupport argument type")]))

      (define processed-args (map convert-arg args))
      (LLVMLinkInJIT)
      (define ee (LLVMCreateExecutionEngineForModule module))
      (define output (LLVMRunFunction ee fun processed-args))
      (LLVMGenericValueToInt output #t)
    )))
