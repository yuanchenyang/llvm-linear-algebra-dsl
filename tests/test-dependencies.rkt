#lang racket

(require rackunit)
(require fast-math/nodes)

(define add-func
  (func-decl int "add" (list (param "x" int) (param "y" int))
          (block '() (return (add (symbol "x") (symbol "y"))))))

(test-begin
   "Test loop-add dependencies"
   (define deps (node-dependencies add-func))
   (define expected-deps '())
   (check-equal? deps expected-deps))

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define d (symbol "d"))
(define i (symbol "i"))
(define j (symbol "j"))

(define loop-1-assgn-1 (assign c a))
(define loop-1-assgn-2 (assign c a))
(define loop-1-loop-inner (for-node j (num 0) (num 10) (num 1) (list loop-1-assgn-2) '()))
(define loop-1-loop-outer (for-node i (num 0) (num 10) (num 1) (list loop-1-loop-inner) '()))

(define loop-add
  (func-decl int "loop-add" (list (param "a" int) (param "b" int))
             (block (list loop-1-assgn-1 loop-1-loop-outer) (return c))))

;(define accesses (node-accesses loop-add))
;(define reads (car accesses))
;(define writes (cadr accesses))
;(display "reads: ")
;(displayln (set->list reads))
;(display "writes: ")
;(displayln (set->list writes))

(test-begin
   "Test loop-add dependencies"
   (define deps (node-dependencies loop-add))
   (define expected-deps (list '() (list loop-1-assgn-1)))
   (check-equal? deps expected-deps))

(define loop-2-assgn-1 (assign c a))
(define loop-2-assgn-2 (assign d (num 0)))
(define loop-2-assgn-3 (assign d (add d a)))
(define loop-2-assgn-4 (assign c (add c d)))
(define loop-2-loop-2 (for-node j (num 0) (num 10) (num 1)
                                (list loop-2-assgn-3 loop-2-assgn-4) '()))
(define loop-2-loop-1 (for-node i (num 0) (num 10) (num 1)
                                (list loop-2-assgn-2 loop-2-loop-2) '()))

(define loop-accum
  (func-decl
   int
   "loop-accum"
   (list (param "a" int) (param "b" int))
   (block
    (list loop-2-assgn-1 loop-2-loop-1)
    (return c))))

(test-begin
 "Test loop-add dependencies"
 (define deps (node-dependencies loop-accum))
 (define expected-deps (list '() (list loop-2-assgn-1)))
 (check-equal? deps expected-deps))
