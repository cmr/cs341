#lang racket

; cons takes an element and a list and adds the element to the front of the list
; car  takes a list and returns its first element
; cdr  takes a list and returns that list minus its first element
; (define null? (ls) (equal? ls '()))

(define list-from-to
  (lambda (low high)
    (if (> low high)
        '()
        (cons low (list-from-to (+ low 1) high)))))
 
(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (length (cdr ls))))))

(define append
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b)))))

(define reverse
  (lambda (a)
    (if (null? a)
        a
        (append (reverse (cdr a)) (cons (car a) '())))))

(define prefix
  (lambda (a b)
    (cond ((null? a) #t)
          ((null? b) #f)
          (else (if (equal? (car a) (car b))
                    (prefix (cdr a) (cdr b))
                    #f)))))

(define subsequence
  (lambda (a b)
    (cond ((null? b) #f)
          ((prefix a b) #t)
          (else (subsequence a (cdr b))))))