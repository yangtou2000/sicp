(define nil '())
(define true #t)
(define false #f)
 
(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1 ) set2)
           (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1)) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (element-of-multiset? x set)
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-multiset? x (cdr set)))))

(define (adjoin-multiset x set)
  (cons x set))

(define (intersection-muiltiset set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-multiset? (car set1 ) set2)
           (cons (car set1) (intersection-multiset (cdr set1) set2)))
        (else (intersection-multiset (cdr set1) set2))))

(define (union-multiset set1 set2)
  (cons (car set1) (union-multiset (cdr set1) set2)))

(define (element-of-sortedset? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((> x (car set)) false)
        (else (element-of-sortedset? x (cdr set)))))

(define (adjoin-sortedset x set)
  (if (null? set) 
    false
    (let ((a (car set)))
      (cond ((= a x) set)
            ((< a x) (cons a (adjoin-sortedset x (cdr set))))
            ((> a x) (cons x set))))))

(define (intersection-of-sortedset set1 set2)
  (if (or (null? set1) (null? set2)) 
    nil
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond  ((= x1 x2) (cons x1 (intersection-of-sortedset (cdr set1) (cdr set2))))
             ((< x1 x2) (intersection-of-sortedset (cdr set1) set2))
             ((> x1 x2) (intersection-of-sortedset set1 (cdr set2)))))))

(define (union-sortedset set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-sortedset (cdr set1) (cdr set2))))
                  ((> x1 x2) (cons x2 (union-sortedset set1 (cdr set2))))
                  ((< x1 x2) (cons x1 (union-sortedset (cdr set1) set2))))))))


