(define nil (quote ()))
(define True #t)
(define False #f)

(define (for-each proc sequence)
  (if (null? sequence)
      True
      (let ()
           (proc (car sequence))
           (for-each proc (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define empty-board nil)
(define (adjoin-position new-row col rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? k positions)
  (define (check-iter row rest-positions offset)
    (if (null? rest-positions)
        True
        (let ((position (car rest-positions)))
          (if (or (= row position)
                  (= row (+ position offset))
                  (= row (- position offset)))
	      False
	      (check-iter row (cdr rest-positions) (+ 1 offset))))))
  (check-iter (car positions) (cdr positions) 1))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0) 
        (list empty-board) 
        (filter 
         (lambda (positions) (safe? k positions)) 
         (flatmap 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))) 
  (queen-cols board-size))

(define (louis-queens board-size)
  (define (louis-queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-positions)
                     (adjoin-position new-row k rest-of-positions))
                   (louis-queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (louis-queen-cols board-size))
