#lang racket

(require rackunit)
(require fast-math/analysis)
(require fast-math/nodes)
(require fast-math/transforms)

(define tree
  (func-decl
   int-ptr
   "func1"
   (list (param "a" int-ptr) (param "b" int-ptr) (param "c" int-ptr) (param "d" int-ptr))
   (block
    (list
     (assign (symbol "z") (num 1))
     (for-loop (symbol "u1") 0 2 1
               (list
                (for-loop (symbol "u2") 0 2 1
                          (list
                           (assign
                            (array-reference  (symbol "c") (add (symbol "u2")
                                                                (mul (num 2) (symbol "u1"))))
                            (add
                             (array-reference (symbol "b") (add (symbol "u2")
                                                                (mul (num 2) (symbol "u1"))))
                             (array-reference (symbol "a") (add (symbol "u2")
                                                                (mul (num 2) (symbol "u1")))))))
                          (list pragma-ignore-loop-deps)))
               (list pragma-ignore-loop-deps))
     (assign (symbol "y") (num 9))
     (for-loop (symbol "y1") 0 2 1
               (list
                (for-loop (symbol "y2") 0 2 1
                          (list
                           (assign
                            (array-reference  (symbol "d") (add (symbol "y2")
                                                                (mul (num 2) (symbol "y1"))))
                            (add
                             (array-reference (symbol "c") (add (symbol "y2")
                                                                (mul (num 2) (symbol "y1"))))
                             (array-reference (symbol "a") (add (symbol "y2")
                                                                (mul (num 2) (symbol "y1")))))))
                          (list pragma-ignore-loop-deps)))
               (list pragma-ignore-loop-deps)))
    (return (symbol "d")))))


;; (define bb_list (liveness-analyze (build-basic-blocks (func-decl-body tree))))
;; (define fused_bb_list (liveness-analyze (build-basic-blocks (func-decl-body (fusion-pass tree)))))

(require racket/pretty)
;; (pretty-print (liveness-analyze (build-basic-blocks (fusion-pass (loop-compression tree)))))
(pretty-print (mem-to-reg (fusion-pass (loop-compression tree))))
