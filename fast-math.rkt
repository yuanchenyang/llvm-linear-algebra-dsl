#lang racket
;; (matrix [[expr ...+] ...+])
;; 
;; Entrywise Ops
;; (+ M N) -> (Matrix Number)
;; (- M N) -> (Matrix Number)
;; (* M N) -> (Matrix Number)
;; (/ M N) -> (Matrix Number)
;;   M: (Matrix Number)
;;   N: (Matrix Number)
;; 
;; (+ M N) -> (Matrix Number)
;; (- M N) -> (Matrix Number)
;; (* M N) -> (Matrix Number)
;; (/ M N) -> (Matrix Number)
;;   M: (Matrix Number)
;;   N: Number
;; 
;; (+ M N) -> (Matrix Number)
;; (- M N) -> (Matrix Number)
;; (* M N) -> (Matrix Number)
;; (/ M N) -> (Matrix Number)
;;   M: Number
;;   N: (Matrix Number)
;; 
;; (+ M N) -> Number
;; (- M N) -> Number
;; (* M N) -> Number
;; (/ M N) -> Number
;;   M: Number
;;   N: Number
;; 
;; Special
;; (map f M) -> (Matrix Number)
;;   f: (Number -> Number)
;;   M: (Matrix Number)
;; (map f M N ...) -> (Matrix Number)
;;   f: (Number Number ... -> Number)
;;   M: (Matrix Number)
;;   N: (Matrix Number)
;; 
;; (transpose M) -> (Matrix Number)
;;   M: (Matrix Number)
