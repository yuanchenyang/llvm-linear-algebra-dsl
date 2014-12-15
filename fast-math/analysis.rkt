#lang racket

(require "nodes.rkt")
(provide liveness-analyze
         build-basic-blocks)

(struct basic-block (statements) #:transparent)

(define (bb-builder curr so-far)
  (if (for-node? curr)
      (append (list '()
                    (flatten (build-basic-blocks (for-node-body curr)))
                    (basic-block (car so-far)))
              (cdr so-far))
      (cons (cons curr (car so-far)) (cdr so-far))))

(define (build-basic-blocks body)
  (let ([bbs (foldr bb-builder (list '()) body)])
    (flatten (cons (basic-block (car bbs)) (cdr bbs)))))

(define (liveness-analyze curr live-outs)
  (let ([gen (build-gen curr)])
    '()))

(define (build-gen tree)
  '())
