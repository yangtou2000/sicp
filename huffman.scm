(define nil '())
(define true #t)
(define false #f)

;(define (memq x set)
;  (cond ((null? set) false)
;        ((eq? x (car set)) true)
;        (else (memq x (cdr set)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left 
        right 
        (append (symbol left) (symbol right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbol tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (symbol-on-tree? x tree)
  (memq x (symbol tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit current)
  (cond ((= bit 0) (left-branch current))
        ((= bit 1) (right-branch current))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;add elment to list of symbol sorted by weight
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set)) ;; <= or <?
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (define (succssive-merge leaf-sets)
    (cond ((null? leaf-sets) nil)
          ((= 1 (length leaf-sets)) (car leaf-sets))
          (else 
           (let ((minimum1 (car leaf-sets))
                 (minimum2 (cadr leaf-sets))
                 (remain-sets (cddr leaf-sets)))
             (let ((new-leaf (make-code-tree minimum1 minimum2)))
               (succssive-merge (adjoin-set new-leaf remain-sets)))))))
  (succssive-merge (make-leaf-set pairs)))

(define (encode-symbol x tree)
  (cond ((not (symbol-on-tree? x tree))
         (error "symbol is not on this tree"))
        ((leaf? tree) nil)
        (else (if (symbol-on-tree? x (left-branch tree))
                  (cons 0 (encode-symbol x (left-branch tree)))
                  (cons 1 (encode-symbol x (right-branch tree)))))))

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
 
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) 
        nil
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))