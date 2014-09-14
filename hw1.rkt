#lang racket

; I use racket builtins such as andmap/ormap/filter-map/and etc which are
; pretty easy to define but I don't do so for brevity.

;;;;;;;; "database" used to direct the "AI"

(define *endings*
  '((i quit)))

(define *strong-cues*
  '( ( ((the names) (their names))
       ((whos on first whats on second i dont know on third)
        (whats on second whos on first i dont know on third)) )
     
     ( ((suppose) (lets say) (assume))
       ((okay) (why not) (sure) (it could happen)) )
     
     ( ((i dont know))
       ((third base) (hes on third)) )
     ))

(define *weak-cues*
  '( ( ((who) (whos) (who is))
       ((first-base)
        ((thats right) (exactly) (you got it)
                       (right on) (now youve got it)))
       ((second-base third-base)
        ((no whos on first) (whos on first) (first base))) )
     ( ((what) (whats) (what is))
       ((first-base third-base)
        ((hes on second) (i told you whats on second)))
       ((second-base)
        ((right) (sure) (you got it right))) )
     ( ((whats the name))
       ((first-base third-base)
        ((no whats the name of the guy on second)
         (whats the name of the second baseman)))
       ((second-base)
        ((now youre talking) (you got it))))
     ))

(define *hedges*
  '((its like im telling you)
    (now calm down)
    (take it easy)
    (its elementary lou)
    (im trying to tell you)
    (but you asked)))

(define *context-words*
  '( ( ((first)) first-base )
     ( ((second)) second-base )
     ( ((third)) third-base )))

; tolerance for fuzzy sublist matching. the higher, the more tolerant
; cue matching is of errors.
(define fuzz-factor 2)

;;;;;;;; provided mainloop

(define whos-on-first-loop 
  (lambda (old-context)
    (let ((costellos-line (read)))
      (let ((new-context (get-context costellos-line old-context)))
        (let ((strong-reply (try-strong-cues costellos-line)))
          (let ((weak-reply (try-weak-cues costellos-line new-context)))
            (cond ((not (null? strong-reply))
                   (writeln strong-reply)
                   (whos-on-first-loop (get-context strong-reply new-context)))
                  ((not (null? weak-reply))
                   (writeln weak-reply)
                   (whos-on-first-loop (get-context weak-reply new-context)))
                  ((wants-to-end? costellos-line)
                   (wrap-it-up))
                  (else 
                   (writeln (hedge))
                   (whos-on-first-loop new-context)))))))))

;;;;;;;; helpers

(define writeln displayln)

(define is-in-list?
  (λ (elem ls)
    (cond ((null? ls) #f)
          ((equal? (car ls) elem) #t)
          (else (is-in-list? elem (cdr ls))))))

(define select-any-from-list
  (λ (ls)
    (cond ((null? ls) '())
          (else (list-ref ls (inexact->exact (floor (* (random) (length ls)))))))))

; checks if a is a sublist of b, but won't allow more than n
; elements between elements of a that are found in b, total.
; approximates, poorly, fuzzy matching
(define fuzzy-sublist?
  (λ (a b n)
    (define inner
      (λ (a_ b n_ prev-true?)
        (cond ((null? a_) #t)
              ((null? b) #f)
              ((= n 0) (inner a b n_ #f))
              ((equal? (car a_) (car b)) (inner (cdr a_) (cdr b) n_ prev-true?))
              (prev-true? (inner a_ (cdr b) n_ #f))
              (else (inner a_ (cdr b) (- n_ 1) #t)))))
    (inner a b n #t)))


(define any-good-fragments?
  (λ (list-of-cues sentence)
    (ormap (λ (cue) (fuzzy-sublist? cue sentence fuzz-factor)) list-of-cues)))

(define safe-car
  (λ (ls)
    (cond ((null? ls) '())
          (else (car ls)))))

;;;;;;;; core logic

(define wants-to-end?
  (λ (costellos-line)
    (is-in-list? costellos-line *endings*)))

(define try-cues 
  (λ (cues sentence)
    (select-any-from-list
     (safe-car (filter-map (λ (x) (and (any-good-fragments? (first x) sentence) (second x))) cues)))))

(define try-strong-cues
  (λ (sentence)
    (try-cues *strong-cues* sentence)))

(define try-weak-cues
  (λ (sentence context)
    (select-any-from-list 
     (select-any-from-list
      (safe-car
       ; *weak-cues* is a list:
       ; first check if the sentence matches anything in the first element in the list
       ; if it does, take the cdr of the list. the remainder is a list of pairs:
       ; - check if the context is in the first element. if so, return the second element
       (filter-map
        (λ (cue) (and
                  (any-good-fragments? (first cue) sentence)
                  (filter-map (λ (tagged-response)
                                (and (is-in-list? context (first tagged-response))
                                     (second tagged-response)))
                              (cdr cue))))
        *weak-cues*))))))


(define get-context
  (λ (sentence old-context)
    (cond ((null? sentence) old-context)
          ((null? *context-words*) old-context)
          (else (let ((new-context 
                       (filter-map 
                        (λ (x) (and (ormap (λ (word) (fuzzy-sublist? word sentence fuzz-factor)) (first x)) (second x)))
                        *context-words*)))
                  (if (null? new-context) old-context (select-any-from-list new-context)))))))

(define hedge
  (λ ()
    (select-any-from-list *hedges*)))

(define wrap-it-up (λ () '()))