(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

(define (make-serializer)
  (let ((lock (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (lock 'acquire)
        (let ((val (apply p args)))
          (lock 'release)
          val))
      serialized-p)))

;exercise 3.47

(define (make-semaphore-1 n)
  (let ((lock (make-mutex)))
    (define (acquire)
      (lock 'acquire)
      (cond ((= n 0)
             (lock 'release)
             (acquire));重复
            ((> n 0)
             (set! n (- n 1))
             (lock 'release)
             'ok)))
    (define (release)
      (lock 'acquire)
      (set! n (+ n 1))
      (lock 'release)
      'ok)
    (define (dispatch req)
      (cond ((eq? req 'acquire) (acquire))
            ((eq? req 'release) (release))
            (else (error "unknow operation MAKE-SEMAPHORE" mode))))
    dispatch))

(define (make-semaphore-2 n)
  (let ((cell (list false)))
    (define (acquire)
      (if (test-and-set! cell)
          (acquire)
          (cond ((= n 0) (clear! cell) 
                         (acquire))
                ((> n 0) (set!  n (- n 1)) 
                         (clear! cell)
                         'ok))))
    (define (release)
      (if (test-and-set! cell)
          (release)
          (begin (set! n (+ n 1)) 
                 (clear! cell)
                 'ok)))    
    (define (dispatch req)
      (cond ((eq? req 'acquire) (acquire))
            ((eq? req 'release) (release))
            (else (error "unknow operation MAKE-SEMAPHORE" mode))))
    dispatch))

