#lang racket

(require fast-math/matrix)
(require fast-math/frontend)
(require fast-math/nodes)

;; Get path to the image from the command line arguments
;;(define arguments (current-command-line-arguments))
;;(unless (= (vector-length arguments) 2)
;;  (printf "Usage: racket demo.rkt frame0 frame1~n")
;;  (exit))
(define frame0-name "image1.png")
(define frame1-name "image1.png")

(require racket/draw)
(require ffi/unsafe)


(define (color->gray color-bm)
  (define color-dc (new bitmap-dc% [bitmap color-bm]))
  (define-values (w h) (send color-dc get-size))
  (define width (exact-floor w))
  (define height (exact-floor h))
  (define pixels (make-bytes (* 4 width height)))
  (define gray-pixels (malloc (_array _double height width)))
  (send color-dc get-argb-pixels 0 0 width height pixels)
  (for ([i (in-range 0 (* 4 width height) 4)])
    (define α (bytes-ref pixels i))
    (define r (bytes-ref pixels (+ i 1)))
    (define g (bytes-ref pixels (+ i 2)))
    (define b (bytes-ref pixels (+ i 3)))
    (define l (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))    
    (ptr-set! gray-pixels _double (/ i 4) l))
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
(define-values (w h) (send (new bitmap-dc% [bitmap frame1]) get-size))
(define height (exact-floor h))
(define width (exact-floor w))
;; (define du (make-zero-matrix "du" height width))
;; (define dv (make-zero-matrix "dv" height width))

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

(define-optimized (plus mat (a mat) (b mat) (c mat))
  (+. (+. a b) c))

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

(define edge-detector-x (make-constant-matrix "e" (list (list -1.0 0.0 1.0)
                                                        (list -2.0 0.0 2.0)
                                                        (list -1.0 0.0 1.0))))
(define edge-detector-y (make-constant-matrix "e" (list (list -1.0 -2.0 -1.0)
                                                        (list 0.0 0.0 0.0)
                                                        (list 1.0 2.0 1.0))))

(define-optimized (edge-detector mat (a mat))
  (+. (*. (convolve. a edge-detector-x) (convolve. a edge-detector-x))
      (*. (convolve. a edge-detector-y) (convolve. a edge-detector-y))))

(define edges (edge-detector gray1))
;; (define Ix (convolve gray1 Gx))
;; (define Iy (convolve gray1 Gy))
;; (define It (subt gray1 gray0))
;; (define Ix2 (mult Ix Ix))
;; (define Iy2 (mult Iy Iy))
;; (define Ixy (mult Ix Iy))
;; (define ubar (convolve du D))
;; (define vbar (convolve dv D))
;; (define numer (compute-num Ix ubar Iy vbar It))
;; (define lam2 (make-constant-matrix "lam2"
;;                                    (for/list ([i (in-range height)])
;;                                      (for/list ([j (in-range width)])
;;                                        .1))))

;; (define (solve du dv iter)
;;   (for/fold ([du-dv (cons du dv)])
;;       ([_ (in-range iter)])
;;     (match-let* ([(cons du dv) du-dv]
;;                  [ubar (convolve du D)]
;;                  [vbar (convolve dv D)]
;;                  [num (compute-num Ix ubar Iy vbar It)]
;;                  [den (plus Ix2 Iy2 lam2)]
;;                  [du (update-vectors ubar Ix num den)]
;;                  [dv (update-vectors vbar Iy num den)])
;;                 (cons du dv))))
;; (solve du dv 10)

;; (define u-v (solve du dv 5))
;; (define u (car u-v))
;; (define v (cdr u-v))

(require (only-in math/array build-array array-make-polar array+ array-magnitude
                  array-all-min array-angle array array-all-max array/ array* array-shape
                  array-map array->list array-sqr array-scale array-sqrt))

;; (define u-arr (build-array (vector height width) (lambda (js)
;;                                                    (match-define (vector j0 j1) js)
;;                                                    (matrix-ref u j0 j1))))
;; (define v-arr (build-array (vector height width) (lambda (js)
;;                                                    (match-define (vector j0 j1) js)
;;                                                    (matrix-ref v j0 j1))))
(define e-arr (array-sqrt (build-array (vector height width) (lambda (js)
                                                               (match-define (vector j0 j1) js)
                                                               (matrix-ref edges j0 j1)))))
(define e-norm
  (let ([max (array-all-max e-arr)])
    (array-scale e-arr (* (/ 1 max) 255))))
;; (define mag-ang (array-make-polar u-arr v-arr))
;; (define mag (array-magnitude mag-ang))
;; (define ang (array-angle mag-ang))
;; (define min-mag (array-all-min mag))
;; (define mag-shift (if (< min-mag 0) (abs min-mag) 0))
;; (define mag-shifted (array+ mag (array mag-shift)))
;; (define mag-max (array-all-max mag-shifted))
;; (define mag-normalized (array-scale (array/ mag-shifted (if (= 0 mag-max) (array 1) (array mag-max))) 255))

(define output (make-object bitmap% width height))
(define output-dc (new bitmap-dc% [bitmap output]))

;; (define mag-list (array->list mag-normalized))
(define mag-list (array->list e-norm))
(define pixels (make-bytes (* 4 width height)))
(for ([i (in-range 0 (* 4 height width) 4)]
      [item mag-list])
  (bytes-set! pixels i 1)
  (bytes-set! pixels (+ i 1) (exact-floor item))
  (bytes-set! pixels (+ i 2) (exact-floor item))
  (bytes-set! pixels (+ i 3) (exact-floor item))
  )

(send output-dc set-argb-pixels 0 0 width height pixels)

(require racket/draw mred)
(define f (new frame% [label "foo"] [width width] [height height]))
(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (inherit get-dc)
    (define/override (on-paint)
      (send (get-dc) draw-bitmap bitmap 0 0))
    (super-new)))

(define the-canvas (new bitmap-canvas% [parent f] [bitmap output]))
(send f show #t)
