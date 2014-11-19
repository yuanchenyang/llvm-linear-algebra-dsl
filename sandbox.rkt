#lang racket

(define (symbolic? e)
  (or (symbol? e)
      (list?   e)))

(define (power x n)
  (if (= n 0)
      '1
      `(* ,x ,(power x (- n 1)))))

(define-syntax-rule (if. pred t f)
  (cond [(symbolic? pred)
         (list 'if pred t f)]
        [pred t]
        [else f]))

(define (=. e1 e2)
  (if (or (symbolic? e1)
          (symbolic? e2))
      (list '= e1 e2)
      (= e1 e2)))

(define (*. e1 e2)
  (if (or (symbolic? e1)
          (symbolic? e2))
      (list '* e1 e2)
      (* e1 e2)))

(define (-. e1 e2)
  (if (or (symbolic? e1)
          (symbolic? e2))
      (list '- e1 e2)
      (- e1 e2)))

(define (power. x n)
  (if. (=. n 0)
       1
       (*. x (power. x (-. n 1)))))

(power. 'x 4)
(power. 4 'x)
