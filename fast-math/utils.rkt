#lang racket

(require fast-math/nodes)

(define uniq 0)

(define (gen-unique-num)
  (set! uniq (+ 1 uniq))
  uniq)

(define (gen-unique-name)
  (string-append "u" (number->string (gen-unique-num))))

(define (gen-unique-symbol)
  (symbol (gen-unique-name)))

(define flatten (lambda (x) (apply append x)))

(define (array-ref-name a)
  (symbol-name (array-reference-arr a)))

(define (group-by cmp lst)
  (foldr (lambda (cur so-far)
           (let ([prev (if (null? so-far) #f (caar so-far))])
             (if (and prev (cmp cur prev))
                 (cons (cons cur (car so-far))
                       (cdr so-far))
                 (cons (list cur)
                       so-far))))
         '()
         lst))

(provide gen-unique-num
         gen-unique-symbol
	 gen-unique-name
         flatten)
