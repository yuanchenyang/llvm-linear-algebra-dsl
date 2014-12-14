#lang racket

(require "nodes.rkt")
(require "transforms.rkt")
(require "matrix.rkt")
(require "utils.rkt")
(require racket/pretty)

(define env
  (make-hash (list (cons "a" (num 1))
                   (cons "b" (num 2))
                   (cons "m" (make-constant-matrix "m" '((1 2 3) (4 5 6)))))))

(define test1
  (add (symbol "a")
       (num 3)))

(define test2
  (add (symbol "b")
       (mul (symbol "a")
            (num 4))))

(define test3
  (add (symbol "a")
       (array-reference (symbol "m")
                        (add (num 2)
                             (symbol "b")))))

(define test4
  (let* ([i (symbol "i")]
         [t (symbol "t")]
         [ti (add i (num 1))]
         [m (symbol "m")]
         [mi (mul i (num 2))])
    (for-unroll env i 2 0 -1
      (assign (array-reference t ti)
              (add (num 3)
                   (array-reference m mi))))))

((constant-fold env) test1)
((constant-fold env) test2)
((constant-fold env) test3)
(pretty-print test4)
