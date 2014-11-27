#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Streams (infinite lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/stream)

; defining a stream (infinite list) of natural numbers
; uses "lazy" evaluation
(define naturals
  (let helper ((n 0))
       (stream-cons n
		    (helper (+ n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mutators for stream and list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-stream->list
; constructs a list of the first n items from a stream
;
(define my-stream->list
  (lambda (str n)
	  (cond ((stream-empty? str) '())
		((zero? n) '())
		(else (cons (stream-first str)
			    (my-stream->list (stream-rest str) (- n 1)))))))

; my-list->stream
; constructs a stream from a list
;
(define my-list->stream
  (lambda (list)
	  (cond ((null? list) empty-stream)
		(else (stream-cons (car list)
				   (my-list->stream (cdr list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; other functions on streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; adding two streams
(define stream-add
  (lambda (str1 str2)
	  (cond ((stream-empty? str1) empty-stream)
                ((stream-empty? str2) empty-stream)
		(else (stream-cons (+ (stream-first str1) (stream-first str2))
				   (stream-add (stream-rest str1) (stream-rest str2)))))))

; map function on streams
(define my-stream-map
  (lambda (func str)
    (cond ((stream-empty? str) empty-stream)
          (else (stream-cons
                  (func (stream-first str))
                  (my-stream-map func (stream-rest str)))))))

; filter function on streams
(define my-stream-filter
  (lambda (test? str)
    (cond ((stream-empty? str) empty-stream)
          ((test? (stream-first str))
           (stream-cons (stream-first str)
                        (my-stream-filter test? (stream-rest str))))
          (else (my-stream-filter test? (stream-rest str))))))

; defining a stream of fibonacci numbers
;  FIB(n) = FIB(n-1) + FIB(n-2) if n > 1
;  FIB(1) = 1
;  FIB(0) = 0
(define fibonaccis
  (letrec
    ((make-stream
       (lambda (a b)
         (stream-cons a (make-stream b (+ a b))))))
    (make-stream 0 1)))

; defining a stream of (positive) rational numbers
; a "rational" number is a pair (a . b) where a and b are positive integers
; note: the stream should not contain any repeats
; example:
;  (1 . 1) (1 . 2) (2 . 1) (1 . 3) (3 . 1) (1 . 4) (2 . 3) (3 . 2) (4 . 1) (1 . 5) ...
(define rationals
  ; Algorithm taken from
  ; http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/rationals.pdf,
  ; which is a *beautiful* functional pearl, rivaling
  ; http://www.mpi-sws.org/~turon/rollback.pdf for my "favorite pearl".
  (letrec
    ((iterate
       (lambda (f x)
         (stream-cons
           x
           (iterate f (f x)))))

     (recip (lambda (x) (list (second x) (first x))))

     (ratsub (lambda (ab cd)
               (match (list ab cd)
                 [(list (list a b) (list c d))
                  (list (- (* d a) (* b c)) (* b d))])))

     (ratge (lambda (ab cd)
              (match (list ab cd)
                [(list (list a b) (list c d))
                 (>= (* d a) (* b c))])))

     (ratfloor (lambda (ab)
                 (letrec
                    ((inner
                      (lambda (a b i)
                        (if (>= a b)
                          (inner (- a b) b (+ i 1))
                          (list a i)))))
                   (inner (first ab) (second ab) 0))))

    (ratadd (lambda (ab cd)
              (match (list ab cd)
                [(list (list a b) (list c d))
                 (list (+ (* a d) (* b c)) (* b d))])))

    (ratadd3 (lambda (ab cd ef)
               (ratadd ab (ratadd cd ef))))

     (properFraction
       (lambda (ab)
         (list (second (ratfloor ab)) (ratsub ab (fromInteger (second (ratfloor ab)))))))

     (fromInteger
       (lambda (a)
         (list a 1)))

     (next (lambda (x)
             (let ((ny (properFraction x)))
               (recip (ratadd3 (fromInteger (first ny)) (list 1 1) (ratsub (list 0 1) (second ny))))))))

    (iterate next (fromInteger 1))))




;;;;;;;;;;;
; test jig
;;;;;;;;;;;

(define test-me
  (lambda (n)
    (list (my-stream->list rationals n)
          (my-stream->list fibonaccis n)
    	  (my-stream->list (my-stream-map sqr naturals) n)
          (my-stream->list (my-stream-filter even? fibonaccis) n))))

(test-me 15)
