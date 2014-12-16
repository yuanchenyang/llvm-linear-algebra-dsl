#lang racket

(require fast-math/matrix)
(require fast-math/frontend)
(require fast-math/nodes)

(define frame0-name "RasBodik.png")

(require racket/draw)
(require ffi/unsafe)

(define (color->rgb color-bm)
  (define color-dc (new bitmap-dc% [bitmap color-bm]))
  (define-values (w h) (send color-dc get-size))
  (define width (exact-floor w))
  (define height (exact-floor h))
  (define pixels (make-bytes (* 4 width height)))
  ;; (define gray-pixels (malloc (_array _double height width)))
  (define red-pixels (malloc (_array _double height width)))
  (define green-pixels (malloc (_array _double height width)))
  (define blue-pixels (malloc (_array _double height width)))
  (send color-dc get-argb-pixels 0 0 width height pixels)
  (for ([i (in-range 0 (* 4 width height) 4)])
    (define α (bytes-ref pixels i))
    (define r (bytes-ref pixels (+ i 1)))
    (define g (bytes-ref pixels (+ i 2)))
    (define b (bytes-ref pixels (+ i 3)))
    (ptr-set! red-pixels _double (/ i 4) (real->double-flonum r))
    (ptr-set! green-pixels _double (/ i 4) (real->double-flonum g))
    (ptr-set! blue-pixels _double (/ i 4) (real->double-flonum b)))
  ;; (define l (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))    
  ;;(ptr-set! gray-pixels _double (/ i 4) (if (= 0 l) 0.0 l)))
  (values (make-matrix-with-ptr "r" height width red-pixels)
          (make-matrix-with-ptr "g" height width green-pixels)
          (make-matrix-with-ptr "b" height width blue-pixels)))

(define frame0 (make-object bitmap% frame0-name))
(define-values (red green blue) (color->rgb frame0))

(define-values (w h) (send (new bitmap-dc% [bitmap frame0]) get-size))
(define height (exact-floor h))
(define width (exact-floor w))

(define edge-detector-x (make-constant-matrix "e" (list (list -1.0 0.0 1.0)
                                                        (list -2.0 0.0 2.0)
                                                        (list -1.0 0.0 1.0))))
(define edge-detector-y (make-constant-matrix "e" (list (list -1.0 -2.0 -1.0)
                                                        (list 0.0 0.0 0.0)
                                                        (list 1.0 2.0 1.0))))

(define-optimized (edge-detector mat (a mat))
  (+. (*. (convolve. a edge-detector-x) (convolve. a edge-detector-x))
      (*. (convolve. a edge-detector-y) (convolve. a edge-detector-y))))

(define e-r (edge-detector red))
(define e-g (edge-detector green))
(define e-b (edge-detector blue))

(require (only-in math/array build-array array-make-polar array+ array-magnitude
                  array-all-min array-angle array array-all-max array/ array* array-shape
                  array-map array->list array-sqr array-scale array-sqrt))

(define e-r-arr (array-sqrt (build-array (vector height width) (lambda (js)
                                                                 (match-define (vector j0 j1) js)
                                                                 (matrix-ref e-r j0 j1)))))
(define e-g-arr (array-sqrt (build-array (vector height width) (lambda (js)
                                                                 (match-define (vector j0 j1) js)
                                                                 (matrix-ref e-g j0 j1)))))
(define e-b-arr (array-sqrt (build-array (vector height width) (lambda (js)
                                                                 (match-define (vector j0 j1) js)
                                                                 (matrix-ref e-b j0 j1)))))
(define e-r-norm
  (let ([max (array-all-max e-r-arr)])
    (array-scale e-r-arr (* (/ 1 max) 255))))

(define e-g-norm
  (let ([max (array-all-max e-g-arr)])
    (array-scale e-g-arr (* (/ 1 max) 255))))

(define e-b-norm
  (let ([max (array-all-max e-b-arr)])
    (array-scale e-b-arr (* (/ 1 max) 255))))

(define output (make-object bitmap% width height))
(define output-dc (new bitmap-dc% [bitmap output]))

(define r-norm (array->list e-r-norm))
(define g-norm (array->list e-g-norm))
(define b-norm (array->list e-b-norm))
(define pixels (make-bytes (* 4 width height)))

(for ([i (in-range 0 (* 4 height width) 4)]
      [r r-norm]
      [g g-norm]
      [b b-norm]
      )
  (bytes-set! pixels i 1)
  (bytes-set! pixels (+ i 1) (exact-floor r))
  (bytes-set! pixels (+ i 2) (exact-floor g))
  (bytes-set! pixels (+ i 3) (exact-floor b)))

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
