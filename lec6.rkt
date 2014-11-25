#lang racket

; objects

; todo: rewrite without set!

(define make-stack
  (lambda ()
    (let ((stack '()))
      (letrec
        ((writeln (lambda (msg) (write msg) (newline)))
         (this (lambda message
                 (cond ((null? message) '())
                       (else
                         (case (car message)
                           ((empty?)
                            (null? stack))
                           ((top)
                            (if (not (null? stack))
                              (car stack)
                              (this 'error "Top: stack is empty!")))
                           ((error)
                            (write "Error: ")
                            (writeln (cadr message)))
                           ((push!)
                            (set! stack (cons (cadr message) stack)))
                           ((pop!)
                            (set! stack (cdr stack)))
                           ))))))
        this))))
