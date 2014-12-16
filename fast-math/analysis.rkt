#lang racket

(require "nodes.rkt")
(require fast-math/utils)
(provide (all-defined-out))

(struct basic-block (statements live-ins live-outs) #:transparent)

(define (bb-builder curr so-far)
  (if (for-node? curr)
      (list '()
            (basic-block (list curr) (set) (set))
            (basic-block (car so-far) (set) (set))
            (cdr so-far))
      (cons (cons curr (car so-far)) (cdr so-far))))

(define (build-basic-blocks input)
  (let* ([defn (func-decl-body input)]
         [stmts (append (block-stmts defn) (list (block-return defn)))]
         [body (map (lambda (x) (basic-block x (set) (set)))
                    (group-by (lambda (x y) (not (or (for-node? x)
                                                     (for-node? y))))
                              stmts))]
         [block (block body '())])
    (struct-copy func-decl input [body block])))

(define (do-liveness-analysis curr bbs-succ-live-ins)
  (match-let* ([(cons bbs live-outs) bbs-succ-live-ins]
               [(list gen kill _) (build-gen curr)]
               [live-ins (set-union gen (set-subtract live-outs kill))])
              (pretty-print gen)
              (pretty-print kill)
              (cons (cons (struct-copy basic-block curr [live-outs live-outs]
                                       [live-ins live-ins]) bbs) (set-union live-ins live-outs))))
(define (liveness-analyze body)
  (struct-copy
   func-decl
   body
   [body (block (car (foldr do-liveness-analysis (cons '() (set)) (block-stmts (func-decl-body body)))) '())]))

(define (build-gen tree)
  (foldl (lambda (curr gen-kill-assigned)
           (match-let ([(list gen kill assigned) gen-kill-assigned]
                       [(list reads writes) (node-accesses curr)])
             (if (for-node? curr)
                 (build-gen (basic-block (for-node-body curr) (set) (set)))
                 (begin
                   (set-subtract! reads assigned)
                   (list
                    (set-union gen reads)
                    (set-union kill writes)
                    (set-union assigned writes)))))
           ) (list (set) (set) (set)) (basic-block-statements tree)))
