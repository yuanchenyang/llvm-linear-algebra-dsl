(require fast-math/frontend)
(require fast-math/matrix)
(require fast-math/nodes)

(define-optimized (test-add mat (a mat) (b mat))
  (+. a (+. b a)))

(define-optimized (test-convolve mat (a mat) (b mat))
  (convolve. a b))

(let ([a (make-constant-matrix "a" '((1 2 3) (4 5 6)))]
      [b (make-constant-matrix "b" '((7 8 9) (10 11 12)))]
      [c (make-constant-matrix "a" '((1 1 1 1 1)
                                     (1 5 5 5 1)
                                     (1 5 5 5 1)
                                     (1 5 5 5 1)
                                     (1 1 1 1 1)))]
      [d (make-constant-matrix "b" '((0 1 2) (3 4 5) (6 7 8)))])
  (matrix-display (test-add a b))
  (matrix-display (test-convolve c d))
  )
