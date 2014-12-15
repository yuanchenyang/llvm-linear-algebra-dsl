#lang racket

(require fast-math/matrix)
(require fast-math/frontend)
(require fast-math/nodes)

;; Get path to the image from the command line arguments
(define arguments (current-command-line-arguments))
(unless (= (vector-length arguments) 2)
  (printf "Usage: racket demo.rkt frame0 frame1~n")
  (exit))
(define frame0-name (vector-ref arguments 0))
(define frame1-name (vector-ref arguments 1))

(require racket/draw)
(require ffi/unsafe)


(define (color->gray color-bm)
  (define color-dc (new bitmap-dc% [bitmap color-bm]))
  (define-values (w h) (send color-dc get-size))
  (define width (exact-floor w))
  (define height (exact-floor h))
  (define pixels (make-bytes (* 4 width height)))
  (define gray-pixels (malloc (_array _int width height)))
  (send color-dc get-argb-pixels 0 0 width height pixels)
  (for ([i (in-range 0 (* 4 width height) 4)])
    (define α (bytes-ref pixels i))
    (define r (bytes-ref pixels (+ i 1)))
    (define g (bytes-ref pixels (+ i 2)))
    (define b (bytes-ref pixels (+ i 3)))
    (define l (exact-floor (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))    
    (ptr-set! gray-pixels _int (/ i 4) l))
  (make-matrix-with-ptr "a" height width gray-pixels))

(define frame0 (make-object bitmap% frame0-name))
(define frame1 (make-object bitmap% frame1-name))
(define gray0 (color->gray frame0))
(define gray1 (color->gray frame1))

#| 
lam = 0.1
Gx = [-1.0/12.0,-8.0/12.0,0.0,8.0/12.0,1.0/12.0]
Gy = [[-1.0/12.0],[-8.0/12.0],[0.0],[8.0/12.0],[1.0/12.0]]
D = [[1.0/12.0, 2.0/12.0, 1.0/12.0],
[2.0/12.0, 0.0,      2.0/12.0],
[1.0/12.0, 2.0/12.0, 1.0/12.0]]
du = float32(0.0) * u
dv = float32(0.0) * v 

Ix = convolve(im2_data, Gx)
Iy = convolve(im2_data, Gy)
It = im1_data - im2_data

Ix2 = Ix * Ix 
IxIy = Ix * Iy
Iy2 = Iy * Iy

for i in range(num_iter):
ubar = convolve(du, D)
vbar = convolve(dv, D)
num = Ix * ubar + Iy * vbar + It
den = Ix2 + Iy2 + lam2
du = ubar - (Ix * num) / den
dv = vbar - (Iy * num) / den 
|# 
(define-values (w h) (send (new bitmap-dc% [bitmap frame0]) get-size))
(define height (exact-floor h))
(define width (exact-floor w))

(define du (make-zero-matrix "du" height width))
(define dv (make-zero-matrix "dv" height width))

(define Gx (make-constant-matrix
            "b" (list (list (/ -1.0 12.0) (/ -8.0 12.0) 0.0 (/ 8.0 12.0) (/ 1.0 12.0)))))
(define Gy (make-constant-matrix
            "b" (list (list (/ -1.0 12.0))
                      (list (/ -8.0 12.0))
                      (list 0.0)
                      (list (/ 8.0 12.0))
                      (list (/ 1.0 12.0)))))
(define D (make-constant-matrix
           "b" (list (list (/ 1.0 12.0) (/ 2.0 12.0) (/ 1.0 12.0))
                     (list (/ 2.0 12.0) 0.0          (/ 2.0 12.0))
                     (list (/ 1.0 12.0) (/ 2.0 12.0) (/ 1.0 12.0)))))

(define-optimized (subt mat (a mat) (b mat))
  (-. a b))

(define-optimized (plus mat (a mat) (b mat))
  (+. a b))

(define-optimized (mult mat (a mat) (b mat))
  (*. a b))

(define-optimized (convolve mat (a mat) (b mat))
  (convolve. a b))

;; num = Ix * ubar + Iy * vbar + It
(define-optimized (compute-num mat (a mat) (b mat) (c mat) (d mat) (e mat))
  (+. e (+. (*. a b) (*. c d))))

;; dv = vbar - (Iy * num) / den 
(define-optimized (update-vectors mat (a mat) (b mat) (c mat) (d mat))
  (-. a (/. (*. b c) d)))

(define Ix (convolve gray1 Gx))
(define Iy (convolve gray1 Gy))
(define It (subt gray1 gray0))
(define Ix2 (mult Ix Ix))
(define Iy2 (mult Iy Iy))
(define Ixy (mult Ix Iy))

(define (solve du dv iter-left)
  (let* ([ubar (convolve du D)]
         [vbar (convolve dv D)]
         [num (compute-num Ix ubar Iy vbar It)]
         [den (plus Ix2 Iy2)] ;; Should be + lam2
         [du (update-vectors ubar Ix num den)]
         [dv (update-vectors vbar Iy num den)])
    (if (> iter-left 0) (solve du dv (- iter-left 1))
        (cons du dv))
    ))

(solve du dv 10)
