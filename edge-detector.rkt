#lang racket

(require fast-math/matrix)
(require fast-math/frontend)
(require fast-math/nodes)

(define frame0-name "valve.png")

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
(define gray0 (color->gray frame0))

(define-values (w h) (send (new bitmap-dc% [bitmap frame0]) get-size))
(define height (exact-floor h))
(define width (exact-floor w))

(define Sx (make-constant-matrix "Sx" (list (list -1.0 0.0 1.0)
                                            (list -2.0 0.0 2.0)
                                            (list -1.0 0.0 1.0))))
(define Sy (make-constant-matrix "Sy" (list (list -1.0 -2.0 -1.0)
                                            (list 0.0 0.0 0.0)
                                            (list 1.0 2.0 1.0))))

(define-optimized (edge-detector mat (a mat))
  (+. (*. (convolve. a Sx) (convolve. a Sx))
      (*. (convolve. a Sy) (convolve. a Sy))))

(define edges (edge-detector gray0))

(require (only-in math/array build-array array-make-polar array+ array-magnitude
                  array-all-min array-angle array array-all-max array/ array* array-shape
                  array-map array->list array-sqr array-scale array-sqrt))

(define e-arr (array-sqrt (build-array (vector height width) (lambda (js)
                                                               (match-define (vector j0 j1) js)
                                                               (matrix-ref edges j0 j1)))))
(define e-norm
  (let ([max (array-all-max e-arr)])
    (array-scale e-arr (* (/ 1 max) 255))))

(define output (make-object bitmap% width height))
(define output-dc (new bitmap-dc% [bitmap output]))

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
