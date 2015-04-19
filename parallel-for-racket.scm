#lang planet neil/sicp
;;#lang racket
;(define (parallel-execute . thunks)
;  (for-each thread thunks))
(define (parallel-execute . procs)  
  (map thread-wait  
       (map (lambda (proc) (thread proc))
            procs)))
(define (make-serializer)
  (let ((mutex (make-semaphore 1)))
    (lambda (p)
      (define (serialized-p . args)
        (semaphore-wait mutex)
        (let ((val (apply p args)))
          (semaphore-post mutex)
          val))
      serialized-p)))