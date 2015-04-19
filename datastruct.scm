#lang planet neil/sicp
(#%require r5rs/init)

(define f
    (lambda (first-value)
        (set! f (lambda (second-value) 0))
        first-value))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)