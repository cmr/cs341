#lang racket

; Core game logic.
;
; Main idea for solving: search the configuration space with A*.

; Game configurations are represented as a vector.
(require racket/vector)

; I'm using the binary heap distributed with Racket for the priority queue,
; but it wouldn't be difficult to reimplement (albiet somewhat less
; efficiently, because I prefer purely functional data structures in
; functional languages. alas).
(require data/heap)

; Dimension of the puzzle
(define n 3)
; Number of milliseconds to perform each slide animation in
(define anim-ms 500)
; An easing function to use to "smooth out" the slide animation. This should
; be a real-valued function from [0, 1], (ease 0) should be 0, and (ease 1)
; should be 1. Anything else is going to look absurd.
;
; See the disclaimer in LICENSE.md, I adapted this from Robert Penner's
; excellent easing reference.
(define ease
  (lambda (t)
    (letrec ((s (* 1.70158 1.525))
             (easeInBack
               (lambda (t s)
                 (* t t (- (* (+ s 1) t) s))))
             (easeOutBack
               (lambda (t s)
                 (- 1 (easeInBack (- 1 t) s)))))
      (* 0.5
         (if (< t 0.5)
           (easeInBack (* 2 t) s)
           (+ 1 (easeOutBack (- (* 2 t) 1) s)))))))

; Swap the values at indices a and b in config.
(define config-swap!
  (lambda (config a b)
    (let ((fst (vector-ref config a))
             (snd (vector-ref config b)))
      (begin
        (vector-set! config a snd)
        (vector-set! config b fst)))))

; Create a new configuration by swapping the values at indices a and b in
; config.
(define config-swap
  (lambda (config a b)
     (let ((config-clone (make-vector (vector-length config))))
       (begin
         (vector-copy! config-clone 0 config)
         (config-swap! config-clone a b)
         config-clone))))

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
      (map (lambda (loc) (config-swap config zero-loc loc)) locs-to-swap))))

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

(define dall
  (lambda ls
    (for ([l ls])
      (display l) (display " "))
      (newline)))

; A "good heuristic" for A* to solve the puzzle efficiently. This is a very
; simple admissable heuristic that counts the number of tiles that are out of
; place.
(define good-heuristic
  (lambda (g n)
    (length
      (filter
        (lambda (i) (not (= (vector-ref g i) (vector-ref n i))))
        (range (vector-length g))))))

(define goal-state
  (lambda (n)
    (list->vector (range (* n n)))))

; Solve a puzzle, returning a list of configurations to move between to reach
; the goal configuration.
(define solve
  (lambda (config)
    (A*
      config
      (goal-state n)
      config-neighbors
      good-heuristic)))

; Generate a random puzzle, by starting at the goal configuration and making
; N(+1?) moves. Returns the list of moves used to make the puzzle.
; A 1 is added randomly because given an even or odd amount of moves, roughly
; half the possible configurations are reachable.
(define random-puzzle
  (lambda (N)
    (set! N (+ N (round (random))))
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
        (goal-state n)
        (goal-state n)
        '()))))

; Execute a move in the given direction for the given configuration, returning
; the resulting configuration (possibly unchanged, if the move was illegal).
(define config-move
  (lambda (config dir)
    (let* ((zero-loc (vector-member 0 config))
           (index-to-swap
             (cond ((equal? dir 'left) (+ zero-loc 1))
                   ((equal? dir 'right) (- zero-loc 1))
                   ((equal? dir 'up) (+ zero-loc n))
                   ((equal? dir 'down) (- zero-loc n))))
           (legal? (and (>= index-to-swap 0) (< index-to-swap (* n n))))
           (swapped (if legal?
                      (config-swap config zero-loc index-to-swap)
                      config))
           (neighbors (config-neighbors config)))

      (if (member swapped neighbors)
        swapped
        config))))

(define config->string
  (lambda (config)
    (string-join
      (map
        (lambda (y)
          (string-join
            (map
              (lambda (x)
                (string-append
                  (number->string (vector-ref config (+ (* y n) x)))))
              (range n))
            " "))
        (range n))
      (string #\newline))))

; --------- INTERFACE ------------
; The amount of mutable state here succcccks but can't really avoid it.
; I find it somewhat difficult to reason about it, but it's just a state
; machine. In general, state transitions happen due to an input event, and
; when we redraw we use the current state to determine how to do that.

(require racket/gui/base)
(require racket/draw)

; Finds the value that has been moved so that we don't draw it during the
; animation (we set that cell to 0)
(define find-moved-value
  (lambda (a b)
    (let ((idx (vector-member 0 a)))
      (vector-ref b idx))))

; Finds the direction key that would need to be pressed to move from state1 to
; state2, or '() if it's not possible.
(define direction-moved
  (lambda (state1 state2)
    (let* ((moved-value (find-moved-value state1 state2))
           (a (vector-member moved-value state1))
           (b (vector-member moved-value state2))
           (diff (- b a)))
      (cond
        ((equal? diff n) 'down)
        ((equal? diff (- n)) 'up)
        ((equal? diff 1) 'right)
        ((equal? diff (- 1)) 'left)))))

; A canvas to handle window resizes
(define my-canvas%
  (class canvas%
    (init start-config)
    (define start-anim-time 0)
    (define current-state 'waiting)
    ; size of each tile in the puzzle
    (define b 0)
    ; spacing between adjacent tiles
    (define s 0)
    (define config start-config)
    (define next-config config)
    (define moving-val 0)
    (define orig-x 0)
    (define orig-y 0)
    (define xfunc +)
    (define yfunc +)
    (define winning #f)
    (define current-anim-timer (new timer%
                                    [notify-callback
                                      (lambda ()
                                        (send this refresh-now))]))
    (define key-press-timer
      (new timer%
           [notify-callback
             (lambda ()
               (if (or (equal? path 'empty) winning)
                 (send key-press-timer stop)
                 (if (null? path)
                   '()
                   (let ((next (car path)))
                     (set! path (cdr path))
                     (move-to next)))))]))

    (define path '())

    (define move-to
      (lambda (state)
        (let ((key (direction-moved config state)))
          (send this on-char (new key-event% [key-code key])))))

    (define reset-state
      (lambda ()
        (set! start-anim-time 0)
        (set! current-state 'waiting)
        (if (equal? config (list->vector (range (* n n))))
          (set! winning #t)
          (set! winning #f))
        (set! next-config config)
        (set! moving-val 0)
        (set! orig-x 0)
        (set! orig-y 0)
        (send current-anim-timer stop)))

    (define draw-tile
      (lambda (dc x y val xoff yoff)
        (send dc set-text-foreground "black")
        (let*-values
          ([(text) (number->string val)]
           [(tw th _ __) (send dc get-text-extent text)]
           [(x2) (+ (* s (+ x 1)) (* b x) xoff)]
           [(y2) (+ (* s (+ y 1)) (* b y) yoff)])
          (send dc draw-rectangle x2 y2 b b)
          (send dc draw-text text
                (- (+ x2 (/ b 2)) (/ tw 2))
                (- (+ y2 (/ b 2)) (/ th 2))))))

    (define draw-winning
      (lambda (dc)
        (send dc set-text-foreground "green")
        (let-values ([(w h) (send (send this get-parent) get-client-size)])
          (let ((text "You've won! Press n to play again, q to quit"))
            (let-values ([(tw th _ __) (send dc get-text-extent text)])
              (send dc draw-text text
                    (- (/ w 2) (/ tw 2))
                    (- (/ h 2) (/ th 2))))))))

    ; Draw the entire board with each value at its "natural" position
    (define draw-board
      (lambda (dc)
        (let ((c 0))
          (send dc erase)
          (send dc set-brush "red" 'solid)
          (sequence-for-each
            (lambda (v)
              (unless (= v 0)
                (let*-values
                  ([(x) (modulo c n)]
                   [(y) (truncate (/ c n))])
                  (draw-tile dc x y v 0 0)))
              (set! c (+ c 1)))
            (in-vector config))
          (when winning (draw-winning dc)))))

    (define identity1
      (lambda (a . b)
        a))

    (define draw-anim
      (lambda (dc)
        (let* ((dt (- (current-inexact-milliseconds) start-anim-time))
               (dt/n (min (/ dt anim-ms) 1))
               (d (+ b s))
               (dist (* (ease dt/n) d)))
          (if (>= dt/n 1)
            (begin
              (set! config next-config)
              (reset-state)
              (draw-board dc))
            (begin
              (draw-board dc)
              (draw-tile
                dc
                orig-x
                orig-y
                moving-val
                (xfunc 0 dist)
                (yfunc 0 dist)))))))

    (define/override (on-size w h)
      (let ((w (min w h)))
        (set! s (/ (* 0.10 w) (+ n 1)))
        (set! b (/ (* 0.90 w) n))
        (send (send this get-parent) resize w w)
        (send this refresh-now)))

    (define/override (on-char ch)
      (let ((key (send ch get-key-code)))
        (when (equal? current-state 'waiting)
          (when
            (or (equal? key 'up) (equal? key 'right) (equal? key 'down)
                 (equal? key 'left))
             (set! next-config (config-move config key))
             (unless (equal? (send ch get-time-stamp) 0)
               (set! path 'empty))
             (if (equal? config next-config)
               (begin
                 (reset-state)
                 (display "Illegal move! Going from state...") (newline)
                 (display  (config->string config)) (newline)
                 (display "in direction ") (print key) (newline))
               (begin
                 (set! start-anim-time (current-inexact-milliseconds))
                 (set! current-state 'animating)
                 (set! moving-val (find-moved-value config next-config))
                 (set! xfunc (match key
                               ['right +]
                               ['left -]
                               ['up identity1]
                               ['down identity1]))
                 (set! yfunc (match key
                               ['right identity1]
                               ['left identity1]
                               ['up -]
                               ['down +]))
                 (set! orig-x (modulo (vector-member moving-val config) n))
                 (set! orig-y (truncate (/ (vector-member moving-val config) n)))
                 (vector-set! config (vector-member 0 next-config) 0)
                 (send current-anim-timer start 1)
                 (send this refresh-now)))))
        (when (equal? key #\n)
          (set! path 'empty)
          (reset-state)
          (set! config (last (random-puzzle 200)))
          (send this refresh-now))
        (when (or (equal? key #\q) (equal? key 'escape))
          (reset-state)
          (set! path 'empty)
          (exit))
        (when (equal? key #\s)
          (reset-state)
          (set! path (solve config))
          ; give it some wiggle room
          (send key-press-timer start (+ anim-ms 50)))))

    (define/override (on-paint)
      (let ((dc (send this get-dc)))
        (if (equal? current-state 'waiting)
          (draw-board dc)
          (draw-anim dc))))

    (super-new)))

(define frame (new frame% [label "8-Puzzle"]
                   [width 500] [height 500]))
(define canvas (new my-canvas% [start-config (last (random-puzzle 200))]
                    [parent frame]))

(send frame show #t)

(define play-me
  (lambda ()
    (send canvas on-char (new key-event% [key-code #\s]))))
