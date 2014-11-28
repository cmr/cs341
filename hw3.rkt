#lang racket/base

; Main idea: search the configuration space with A*.

; Game configurations are represented as a vector.
(require racket/vector)

; I'm using the binary heap distributed with Racket for the priority queue,
; but it wouldn't be difficult to reimplement (albiet somewhat less
; efficiently, because I prefer purely functional data structures in
; functional languages. alas).
(require data/heap)

(define n 3)

(define config-swap!
  (lambda (config a b)
    (letrec ((fst (vector-ref config a))
             (snd (vector-ref config b)))
      (begin
        (vector-set! config a snd)
        (vector-set! config b fst)))))

(define config-neighbors
  (lambda (config)
    (let* ((zero-loc (vector-member 0 config))
           (on-edge (cond ((< zero-loc n) 'top)
                          ((= (modulo (- zero-loc 1) n) 0) 'right)
                          ((> (* n (- n 1)) zero-loc) 'bottom)
                          ((= (modulo zero-loc n) 0) 'left)
                          (else 'center)))
           (corner (cond ((= zero-loc 0) 'topleft)
                         ((= zero-loc (- n 1)) 'topright)
                         ((= zero-loc (- (* n n) 1)) 'bottomright)
                         ((= zero-loc (* n (- n 1)) 'bottomleft))
                         (else 'center)))
           (corner-loc (lambda (corner)
                         (cond
                           ((equal? corner 'topleft) (list 1 n))
                           ((equal? corner 'topright)
                            (list (- n 2) (- (* n 2) 1)))
                           ((equal? corner 'bottomright)
                            (list (- (* n n) 2) (- (* n (- n 1)) 1)))
                           ((equal? corner 'bottomleft)
                            (list (* n (- n 2)) (+ (* n (- n 1)) 1)))
                           ((equal? corner 'center) '()))))
           (corner-swap (corner-loc corner))
           (locs-to-swap (cond
                           ((not (equal? corner-swap '())) corner-swap)
                           ((equal? on-edge 'center)
                            (list (- zero-loc 1) (+ zero-loc 1)
                                  (- zero-loc n) (+ zero-loc n)))
                           ((or (equal? on-edge 'top)
                                (equal? on-edge 'bottom))
                              (list (- zero-loc 1) (+ zero-loc 1)
                                    (+ zero-loc n)))
                           ((equal? on-edge 'left)
                            (list (+ zero-loc n) (- zero-loc n)
                                  (+ zero-loc 1)))
                           ((equal? on-edge 'right)
                            (list (+ zero-loc n) (- zero-loc n)
                                  (- zero-loc 1))))))
      (map (lambda (loc)
             (let ((config-clone (make-vector (vector-length config))))
               (begin
                 (vector-copy! config-clone 0 config)
                 (config-swap! config-clone zero-loc loc) config-clone)))
           locs-to-swap))))

