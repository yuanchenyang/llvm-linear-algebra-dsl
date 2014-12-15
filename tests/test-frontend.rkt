#lang racket

(require fast-math/frontend)
(require fast-math/matrix)
(require fast-math/nodes)
(provide frontend-tests)

(define-optimized (test-add mat (a mat) (b mat))
  (+. a (+. b a)))

(define-optimized (test-convolve mat (a mat) (b mat))
  (convolve. a b))

;; (let ([c (make-constant-matrix "a" '((1.0 1.0 1.0 1.0 1.0)
;;                                      (1.0 5.0 5.0 5.0 1.0)
;;                                      (1.0 5.0 5.0 5.0 1.0)
;;                                      (1.0 5.0 5.0 5.0 1.0)
;;                                      (1.0 1.0 1.0 1.0 1.0)))]
;;       [d (make-constant-matrix "b" '((0.0 1.0 2.0)
;;                                      (3.0 4.0 5.0)
;;                                      (6.0 7.0 8.0)))])
;;   (matrix-display (test-convolve c d)))

(require rackunit)

(define frontend-tests
  (test-suite
   "Frontend tests"
   (test-case
    "Test add"
    (let* ([a (make-constant-matrix "a" '((1.0 2.0 3.0) (4.0 5.0 6.0)))]
           [b (make-constant-matrix "b" '((7.0 8.0 9.0) (10.0 11.0 12.0)))]
           [actual (test-add a b)])
      (check-equal? (matrix-ref actual 0 0) 9.0)
      (check-equal? (matrix-ref actual 0 1) 12.0)
      (check-equal? (matrix-ref actual 0 2) 15.0)
      (check-equal? (matrix-ref actual 1 0) 18.0)
      (check-equal? (matrix-ref actual 1 1) 21.0)
      (check-equal? (matrix-ref actual 1 2) 24.0)))))
