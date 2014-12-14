#lang racket

(require rackunit)
(require fast-math/backend)
(require fast-math/nodes)
(require fast-math/matrix)

(define add-func
  (func-decl int "add" (list (param "x" int) (param "y" int))
          (block '() (return (add (symbol "x") (symbol "y"))))))

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define d (symbol "d"))
(define i (symbol "i"))
(define j (symbol "j"))

(define loop-add
  (func-decl
   int
   "loop-add"
   (list (param "a" int) (param "b" int))
   (block
    (list
     (assign c a)
     (for-node i (num 0) (num 10) (num 1)
               (for-block j 0 10 1
                          (list (assign c (add c a))))
               (list pragma-ignore-loop-deps)))
    (return c))))

(test-begin
   "Test simple add"
   (check-eq? ((do-math add-func) 5 2) 7))

(test-begin
   "Test for loop"
   (check-eq? ((do-math loop-add) 10 10) 1010))


(define loop-accum
  (func-decl
   int
   "loop-accum"
   (list (param "a" int) (param "b" int))
   (block
    (list
     (assign c a)
     (for-node i (num 0) (num 10) (num 1)
	       (list
		(assign d (num 0))
		(for-node j (num 0) (num 10) (num 1)
			  (list
			   (assign d (add d a))
			   (assign c (add c d)))
                          (list pragma-ignore-loop-deps)))
               (list pragma-ignore-loop-deps)))
    (return c))))

(test-begin
   "Test loop variable"
   (check-eq? ((do-math loop-accum) 4 8) 2204))

(define matrix-add
  (func-decl
   int-ptr
   "elementwise-matrix-add"
   (list (param "a" int-ptr) (param "b" int-ptr))
   (block
    (list
     (allocate (symbol "c") int-ptr 2 2)
     (for-node (symbol "u1") (num 0) (num 2) (num 1)
	(for-block (symbol "u2") 0 2 1
	 (list
	   (assign
	    (array-reference  (symbol "c") (add (symbol "u2") (mul (num 2) (symbol "u1"))))
	    (add
	     (array-reference (symbol "b") (add (symbol "u2") (mul (num 2) (symbol "u1"))))
	     (array-reference (symbol "a") (add (symbol "u2")
                                                (mul (num 2) (symbol "u1")))))))
         (list pragma-ignore-loop-deps))
        (list pragma-ignore-loop-deps)))
    (return (symbol "c")))))


(define A (make-constant-matrix "B" (list (list 1 3) (list 4 7))))
(define B (make-constant-matrix "C" (list (list 2 2) (list 5 6))))

(test-begin
   "Test matrix-add"
   (let ([C ((do-math matrix-add) A B)])
     (check-eq? (matrix-ref C 0 0) 3)
     (check-eq? (matrix-ref C 0 1) 5)
     (check-eq? (matrix-ref C 1 0) 9)
     (check-eq? (matrix-ref C 1 1) 13)))
