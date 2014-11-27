#lang racket

; file->list
; reads a file of characters into a list
;
(define file->list
  (lambda (filename)
    (let ((input-port (open-input-file filename)))
      (letrec
        ((build-input-list
           (lambda ()
             (let ((current-char (read-char input-port)))
               (if (eof-object? current-char)
                 (begin (close-input-port input-port)
                        '())
                 (cons current-char (build-input-list)))))))
        (build-input-list)))))

; list->file
; writes a list of characters to a file
;
(define list->file
  (lambda (filename ls)
    (letrec
      ((write-output-list
         (lambda (outport ls)
           (cond ((port-closed? outport) '())
                 ((null? ls) '())
                 (else (begin (write (car ls) outport)
                              (write-output-list outport (cdr ls))))))))
      (write-output-list (open-output-file filename #:exists 'truncate) ls))))

; (list->file "foo.wat" '(1 2 apple banana 3 4 5))
