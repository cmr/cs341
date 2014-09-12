#lang racket
(require racket/format)

(define assert
  (lambda (bool msg)
    (if bool
        '()
        (raise msg))))

(define assert-eq
  (lambda (a b)
    (assert (equal? a b) (~a a " != " b))))

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (length (cdr ls))))))

(define list-from-to
  (lambda (low high)
    (if (> low high)
        '()
        (cons low (list-from-to (+ low 1) high)))))

(define test-length
  (lambda (n)
    (cond ((equal? n 0) '())
          (else (begin
                  (assert-eq n (length (list-from-to 1 n)))
                  (test-length (- n 1)))))))
(test-length 20)

(define append
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b)))))

(define test-append
  (lambda ()
    (assert-eq '(1 2 3 a b c) (append '(1 2 3) '(a b c)))
    (assert-eq '() (append '() '()))
    (assert-eq '(1 2 3) (append '(1 2 3) '()))
    (assert-eq '(1 2 3) (append '() '(1 2 3)))))
(test-append)

(define reverse
  (lambda (a)
    (if (null? a)
        a
        (append (reverse (cdr a)) (cons (car a) '())))))

(define test-reverse
  (lambda ()
    (define tr
      (lambda (l)
        (assert-eq l (reverse (reverse l)))))
    (tr '())
    (tr '(1))
    (tr '(1 2))
    (tr '(1 2 3))))
(test-reverse)

(define prefix?
  (lambda (a b)
    (cond ((null? a) #t)
          ((null? b) #f)
          (else (if (equal? (car a) (car b))
                    (prefix? (cdr a) (cdr b))
                    #f)))))

(define test-prefix?
  (lambda ()
    (assert-eq #t (prefix? '() '(a b c)))
    (assert-eq #t (prefix? '(a b c) '(a b c d e)))
    (assert-eq #f (prefix? '(a b c) '(x a b y c z)))))
(test-prefix?)

(define subsequence?
  (lambda (a b)
    (cond ((null? b) #f)
          ((prefix? a b) #t)
          (else (subsequence? a (cdr b))))))

(define test-subsequence?
  (lambda ()
    (assert-eq #t (subsequence? '() '(a b c)))
    (assert-eq #t (subsequence? '(a b c) '(x a b c y z)))
    (assert-eq #f (subsequence? '(a b c) '(x a b y c z)))))
(test-subsequence?)

(define sublist?
  (lambda (a b)
    (cond ((null? b) #f)
          ((null? a) #t)
          ((equal? (car a) (car b)) (sublist? (cdr a) (cdr b)))
          (else (sublist? a (cdr b))))))

(define test-sublist?
  (lambda ()
    (assert-eq #t (sublist? '(a b c) '(x a b y c z)))
    (assert-eq #f (sublist? '(a b c) '(x a b y z)))))
(test-sublist?)

(define map
  (lambda (f ls)
    (if (null? ls)
        '()
        (cons (f (car ls)) (map f (cdr ls))))))

(define test-map
  (lambda ()
    (assert-eq '(1 4 9 16) (map sqr '(1 2 3 4)))
    (assert-eq '((c b a) (3 2 1)) (map reverse '((a b c) (1 2 3))))
    (assert-eq '(a b c d) (map (lambda (x) x) '(a b c d)))))
(test-map)

(define filter
  (lambda (pred? ls)
    (cond ((null? ls) '())
          ((pred? (car ls)) (cons (car ls) (filter pred? (cdr ls))))
          (else (filter pred? (cdr ls))))))

(define test-filter
  (lambda ()
    (assert-eq '(1 3 5) (filter odd? '(1 2 3 4 5)))))
(test-filter)

(define foldr
  (lambda (+ initial ls)
    (if (null? ls)
        initial
        (+ (car ls) (foldr + initial (cdr ls))))))

(define accumulate
  (lambda (op base func ls)
    (foldr op base (map func ls))))

(define test-accumulate
  (lambda ()
    (assert-eq 14 (accumulate + 0 sqr '(1 2 3)))
    (assert-eq 36 (accumulate * 1 sqr '(1 2 3)))))
(test-accumulate)

(define remove
  (lambda (elem ls)
    (cond ((null? ls) '())
          ((equal? elem (car ls)) (cdr ls))
          (else (cons (car ls) (remove elem (cdr ls)))))))

(define smallest
  (lambda (ls less?)
    (define inner
      (lambda (elem ils)
        (cond ((null? ils) elem)
              ((less? (car ils) elem) (inner (car ils) (cdr ils)))
              (else (inner elem (cdr ils))))))
    (cond ((null? ls) '())
          (else (inner (car ls) (cdr ls))))))

; straightforward selection sort
(define sort
  (lambda (ls less?)
    (cond ((null? ls) '())
          ((equal? (length ls) 1) (list (car ls)))
          (else (cons (smallest ls less?) (sort (remove (smallest ls less?) ls) less?))))))

(define num-sort
  (lambda (ls)
    (sort ls <=)))

(define make-sort
  (lambda (less?)
    (lambda (ls) (sort ls less?))))

; inner-shuffle shuffles by, for each element in the list, deciding whether to
; keep it at the front or move it to the back. does have problems. for
; example, the first element will only be able to be placed in the first or
; last spot.  so shuffle-me calls it twice, which will actually be capable of
; producing every element in the set of permutations of ls
(define shuffle-me
  (lambda (ls)
    (define inner-shuffle
      (lambda (ls)
        (cond ((null? ls) '())
              ((> (random) 0.5) (cons (car ls) (shuffle-me (cdr ls))))
              (else (append (shuffle-me (cdr ls)) (list (car ls)) )))))
    (inner-shuffle (inner-shuffle ls))))
