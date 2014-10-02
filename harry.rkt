#lang racket

; see do-walk and invocation below

(require graphics/graphics)

; random misc. utilities

; Normalize an integer in [0, 255] to [0, 1]
(define s
  (lambda (c)
    (/ c 255)))

; Create an rgb struct out of pixel values in [0, 255], normalizing to reals
; for the graphics library.
(define srgb
  (lambda (a b c)
    (make-rgb (s a) (s b) (s c))))

(define select-any-from-list
  (lambda (ls)
    (cond ((null? ls) '())
          (else (list-ref ls (inexact->exact (floor (* (random) (length ls)))))))))

; Constrain a value to [lower, upper]
(define clamp
  (lambda (lower upper value)
    (cond ((<= value lower) lower)
          ((>= value upper) upper)
          (else value))))

; The dot product between two vectors in R^2 constrained to [-1, 1] in each
; dimension, with the result normalized to [0, 1].
(define dot
  (lambda (ab cd)
    (let ((a (first ab))
          (b (second ab))
          (c (first cd))
          (d (second cd)))
      ; first, calculate the "raw" dot product.
      ; this result is in [-2, 2], scale to [-1, 1]
      ; normalize to [0, 1] by scaling to [-0.5, 0.5] and adding 0.5
      (+ 0.5 (/ (+ (* a c) (* b d)) 4)))))

; Colors of the smarties Harry drops.
(define smarty-colors
  (list
    (srgb 255 255 255)
    (srgb 255 255 0)
    (srgb 255 0 255)
    (srgb 0 255 255)
    (srgb 0 0 255)
    (srgb 0 255 0)
    (srgb 255 0 0)
   ))

; Directions Harry can travel in.
(define directions
  '(
    (1 0)
    (0 1)
    (1 1)
    (-1 0)
    (0 -1)
    (-1 -1)
    (1 -1)
    (-1 1)
    ))

; Move in a direction, clamping at xbound and ybound, by n steps,
; and return ((newx newy) dir) where dir is the direction that was moved in
(define move
  (lambda (xbound ybound x y n)
    (let ((dir (select-any-from-list directions)))
      (list
       (clamp 0 xbound (+ x (* n (first dir))))
       (clamp 0 ybound (+ y (* n (second dir))))
       dir
       ))))

; Pick a random color and scale its components by weight
;
; idea for coloring: using the dot product of the old direction and the
; new direction to weight the actual color drawn.
; shows up as brigter pixels when he took a wider turn, and darker where
; he took a narrow turn.
(define pick-color
  (lambda (weight)
    (let ((col (select-any-from-list smarty-colors)))
      (make-rgb
        (* weight (rgb-red col))
        (* weight (rgb-green col))
        (* weight (rgb-blue col))))))

; a nice combinator which repeatedly applys func to its own return value
; until a predicate passes. initial arguments supplied.
(define while
  (lambda (test? func . args)
    (let ((result (apply func args)))
      (if (test? result)
          '()
          (while test? func result)))))

(define do-walk
  (lambda (viewport window-width window-height
                    start-x start-y
                    home-x home-y)
    (define drw (draw-pixel viewport))
    (define not-at-home
      (lambda (xy_dir)
        (and (= (first xy_dir) home-x) (= (second xy_dir) home-y))))

    (define walk-randomly
      (lambda (oldxy_dir)
        (let* ((newxy_dir
                 (move window-width window-height
                       (first oldxy_dir) (second oldxy_dir) 1))
               (x (first newxy_dir))
               (y (second newxy_dir))
               (newdir (third newxy_dir)))
          (begin
            (drw (make-posn x y) (pick-color (dot (third oldxy_dir) newdir)))
            newxy_dir))))

    ((draw-viewport viewport) "black")

    (while
      not-at-home
      walk-randomly
      (list start-x start-y '(0 0)))

    (close-viewport viewport)))

(open-graphics)
(do-walk (open-viewport "Harry's Random Walk" 500 500)
         ; X and Y coordinates of the wall Harry can't cross (outer boundary)
         500 500
         ; Where Harry starts
         0 0
         ; Where Harry lives
         500 500)
