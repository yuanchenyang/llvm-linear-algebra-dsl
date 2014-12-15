#lang racket

(require "nodes.rkt")
(provide liveness-analyze
         build-basic-blocks)

(struct basic-block (statements live-ins live-outs) #:transparent)

(define (bb-builder curr so-far)
  (if (for-node? curr)
      (append (list '()
                    (flatten (build-basic-blocks (struct-copy for-node curr
                                                              [body (for-node-body curr)])))
                    (basic-block (car so-far) (set) (set)))
              (cdr so-far))
      (cons (cons curr (car so-far)) (cdr so-far))))

(define (build-basic-blocks input)
  (let* ([body (cond [(func-decl? input)
                      (let ([block (func-decl-body input)])
                        (append (block-stmts block) (list (block-return block))))]
                     [(for-node? input) (for-node-body input)]
                     [else (error "Unsupported node type for build basic blocks")])]
         [bbs (foldr bb-builder (list '()) body)]
         [block-list (filter (lambda (bb) (pair? (basic-block-statements bb)))
                             (flatten (cons (basic-block (car bbs) (set) (set)) (cdr bbs))))])
    (if (func-decl? input)
        (struct-copy func-decl input [body (block block-list '())])
        block-list)))

(define (do-liveness-analysis curr bbs-succ-live-ins)
  (match-let* ([(cons bbs live-outs) bbs-succ-live-ins]
               [(list gen kill _) (build-gen curr)]
               [live-ins (set-union gen (set-subtract live-outs kill))])
              (cons (cons (struct-copy basic-block curr [live-outs live-outs]
                                       [live-ins live-ins]) bbs) (set-union live-ins live-outs))))
(define (liveness-analyze func)
  (struct-copy
   func-decl
   func [body (block
               (car (foldr do-liveness-analysis (cons '() (set))
                           (block-stmts (func-decl-body func)))) '())]))

(define (build-gen tree)
  (foldl (lambda (curr gen-kill-assigned)
           (match-let ([(list gen kill assigned) gen-kill-assigned]
                       [(list reads writes) (node-accesses curr)])
             (set-subtract! reads assigned)
             (list
              (set-union gen reads)
              (set-union kill writes)
              (set-union assigned writes)))
           ) (list (set) (set) (set)) (basic-block-statements tree)))
