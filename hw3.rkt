#lang racket/base

; Main idea: search the configuration space with A*.

; Game configurations are represented as a vector.
(require racket/vector)

; I'm using the binary heap distributed with Racket for the priority queue,
; but it wouldn't be difficult to reimplement (albiet somewhat less
; efficiently, because I prefer purely functional data structures in
; functional languages. alas).
(require data/heap)

; for range in the good-heuristic
(require racket/list)

; Dimension of the puzzle
(define n 3)

; Swap the values at indices a and b in config.
(define config-swap!
  (lambda (config a b)
    (let ((fst (vector-ref config a))
             (snd (vector-ref config b)))
      (begin
        (vector-set! config a snd)
        (vector-set! config b fst)))))

; List all the valid moves from a configuration.
(define config-neighbors
  (lambda (config)
    (let* ((zero-loc (vector-member 0 config))
           (on-edge (cond ((< zero-loc n) 'top)
                          ((= (modulo (+ zero-loc 1) n) 0) 'right)
                          ((>= zero-loc (* n (- n 1))) 'bottom)
                          ((= (modulo zero-loc n) 0) 'left)
                          (else 'center)))
           (corner (cond ((= zero-loc 0) 'topleft)
                         ((= zero-loc (- n 1)) 'topright)
                         ((= zero-loc (- (* n n) 1)) 'bottomright)
                         ((= zero-loc (* n (- n 1))) 'bottomleft)
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
                           ((equal? on-edge 'top)
                            (list (- zero-loc 1) (+ zero-loc 1)
                                  (+ zero-loc n)))
                           ((equal? on-edge 'bottom)
                            (list (- zero-loc 1) (+ zero-loc 1)
                                  (- zero-loc n)))
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

; Do an A* search from start to goal, using expand to find the neighbors of a
; node, and heuristic to approximate the distance from a node to the goal.
;
; Adapted from
; http://www.redblobgames.com/pathfinding/a-star/introduction.html
(define A*
  (lambda (start goal expand heuristic)
    (let ((frontier (make-heap (lambda (a b) (<= (cadr a) (cadr b)))))
             (came-from (make-hash))
             (cost-so-far (make-hash)))
     (letrec ((pq-push (lambda (val pri) (heap-add! frontier (list val pri))))
              (pq-pop (lambda ()
                        (let ((minval (car (heap-min frontier))))
                          (begin
                            (heap-remove-min! frontier)
                            minval))))
              (pq-empty (lambda () (= (heap-count frontier) 0)))
              (reconstruct-path (lambda (hmap end)
                                  (if (or (not (hash-has-key? hmap end))
                                          (null? (hash-ref hmap end)))
                                      '()
                                      (append (reconstruct-path
                                               hmap (hash-ref hmap end))
                                              (list end)))))
              (while (lambda (pred body)
                       (if (pred)
                           (if (equal? (body) 'break)
                               '()
                               (while pred body))
                           '()))))
      (begin
        (pq-push start 0)
        (hash-set! came-from start '())
        (hash-set! cost-so-far start 0)
        (while (lambda () (not (pq-empty)))
               (lambda ()
                 (let ((current (pq-pop)))
                   (if (equal? current goal)
                     'break
                     (for-each
                       (lambda (next)
                         (let ((new-cost
                                 (+ (hash-ref cost-so-far current) 1)))
                           (when (or (not (hash-has-key? cost-so-far next))
                                   (< new-cost (hash-ref cost-so-far next)))
                             (begin
                               (hash-set! cost-so-far next new-cost)
                               (pq-push next (+ new-cost
                                                (heuristic goal next)))
                               (hash-set! came-from next current)))))
                         (expand current))))))
        (reconstruct-path came-from goal))))))

; A "good heuristic" for A* to solve the puzzle efficiently. This is a very
; simple admissable heuristic that counts the number of tiles that are out of
; place.
(define good-heuristic
  (lambda (g n)
    (length
      (filter
        (lambda (i) (not (= (vector-ref g i) (vector-ref n i))))
        (range (vector-length g))))))

; Solve a puzzle, returning a list of configurations to move between to reach
; the goal configuration.
(define solve
  (lambda (config)
    (A*
      config
      (list->vector (range (* n n)))
      config-neighbors
      good-heuristic)))

; Generate a random puzzle, by starting at the goal configuration and making
; N moves. Returns the list of moves used to make the puzzle.
(define random-puzzle
  (lambda (N)
    (letrec ((random-move
               (lambda (n config prev accum)
                 (if (= n 0)
                   accum
                   (let
                     ((next (car (remove prev (shuffle (config-neighbors
                                                         config))))))
                     (random-move
                       (- n 1)
                       next
                       config
                       (append accum (list next))))))))
      (random-move
        N
        (list->vector (range (* n n)))
        (list->vector (range (* n n)))
        '()))))
