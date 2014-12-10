(require "nodes.rkt")

(define a (symbol "a"))
(define b (symbol "b"))
(define c (symbol "c"))
(define i (symbol "i"))
(define j (symbol "j"))

(define tree
  (for i (num 0) (num 10) (num 1)
       (for j (num 0) (num 10) (num 1)
	    (assign (array-ref c (add (mul i (num 10)) j))
		    (add (array-ref a (add (mul i (num 10)) j))
			 (array-ref b (add (mul i (num 10)) j)))))))

(display tree)
