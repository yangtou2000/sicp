#lang planet neil/sicp
(define (double a)
  (* a 2))
 
(define (even? n)
  (= (remainder n 2) 0))
 
(define (halve a)
  (/ a 2))

(define (fast-mul-iter a b d)
  (cond ((= b 0) d)
        ((even? b) (fast-mul-iter (double a) (halve b) d))
        (else (fast-mul-iter a (- b 1) (+ a d)))))
(define (fast-mul a b)
  (fast-mul-iter a b 0))