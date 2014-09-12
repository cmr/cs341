#lang racket

; Basic scheme: define, arithmetic, if, cond, equal?
; Lists: cons, car, cdr, list, null?
; Recursion

(define slow-fib
  (lambda (n)
    (cond ((equal? n 0) 0)
          ((equal? n 1) 1)
          (else (+ (slow-fib (- n 1)) (slow-fib (- n 2)))))))

(define better-fib
  (lambda (n)
    (define iter
      (lambda (m a b)
        (cond ((= m 0) a)
              (else (iter (- m 1) b (+ a b))))))
  (iter n 0 1)))

; let - local bindings
(let ((p 2) (q 3))
  (+ (sqr p) (sqr q)))

; letrec - just like let, but has more restrictions. 
; evaluated in arbitrary order, can't refer to other bindings introduced by the letrec.
; usually used with lambdas, but doesn't have to be!

(letrec
    ((foo (lambda (n) (* n n)))
     (bar 5))
  (foo bar))