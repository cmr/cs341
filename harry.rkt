#lang racket

(require graphics/graphics)

(define s
  (lambda (c)
    (/ c 255)))

(define srgb
  (lambda (a b c)
    (make-rgb (s a) (s b) (s c))))

(define select-any-from-list
  (lambda (ls)
    (cond ((null? ls) '())
          (else (list-ref ls (inexact->exact (floor (* (random) (length ls)))))))))

(define clamp
  (lambda (lower upper value)
    (cond ((<= value lower) lower)
          ((>= value upper) upper)
          (else value))))

(define random-colors
  (list
   (srgb 0 32 128)
   (srgb 32 0 128)
   (srgb 96 0 128)
   (srgb 128 0 96)
   (srgb 0 96 128)
   (srgb 0 47 189)
   (srgb 0 62 250)
   (srgb 128 0 32)
   (srgb 0 128 96)
   (srgb 250 187 0)
   (srgb 189 142 0)
   (srgb 128 32 0)
   (srgb 0 128 32)
   (srgb 32 128 0)
   (srgb 96 128 0)
   (srgb 128 96 0)
   ))

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

(define move
  (lambda (xbound ybound x y)
    (let ((dir (select-any-from-list directions)))
      (list
       (clamp 0 xbound (+ x (first dir)))
       (clamp 0 ybound (+ y (second dir)))
       ))))

; a nice combinator which repeatedly applys func to its own return value
; until a predicate passes. initial arguments supplied.
(define while
  (lambda (test? func . args)
    (let ((result (apply func args)))
      (if (test? result)
          '()
          (while test? func result)))))

(define do-walk
  (lambda (viewport x y)
    (define drw (draw-pixel viewport))
    ((draw-viewport viewport) "black")
    (while
     (lambda (xy) (and (= (first xy) x) (= (second xy) y)))
     (lambda (xy)
       (let ((newxy (move x y (first xy) (second xy))))
         (begin
           (drw (make-posn (first xy) (second xy)) (select-any-from-list random-colors))
           newxy)))
     (list 0 0))
    ; idea for coloring: using the dot product of the old direction and the new direction
    ; and use that to blend with the value on the screen.
    ; should show up, roughly, as a visualization of the average direction Harry traveled
    ; at that point.

    (close-viewport viewport)))

(open-graphics)
(do-walk (open-viewport "Harry's Random Walk" 500 500) 500 500)
