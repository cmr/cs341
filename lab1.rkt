#lang racket

(define id (lambda (n) n))

(define my-sum
  (lambda (n)
    (if (< n 0) 0 (+ n (my-sum (- n 1))))))

(define my-square-sum
  (lambda (n)
    (if (< n 1) 0 (+ (sqr n) (my-square-sum (- n 1))))))

(define my-better-sum 
  (lambda (func n)
    (define helper 
      (lambda (acc m) 
        (if (< m 0)
            acc
            (helper (+ acc (func m)) (- m 1)))))
    (helper 0 n)))

; (define my-square-sum (lambda (n) (my-better-sum sqr n)))

(define my-generic-sum
  (lambda (func)
    (lambda (n) (my-better-sum func n))))

(define test-me
  (lambda (func n)
    (if (< n 0)
        '()
        (cons (func n) (test-me func (- n 1))))))

(define test-me-again
  (lambda (func foo n)
    (if (< n 0)
        '()
        (cons (func foo n) (test-me-again func foo (- n 1))))))