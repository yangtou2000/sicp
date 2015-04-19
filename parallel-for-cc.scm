(define (thunk->thread thunk)
  (let ((thread (make-thread thunk))) (thread-start! thread) thread))
(define (parallel-execute . thunks)
  (let ((threads (map thunk->thread thunks)))
    (lambda () (for-each thread-terminate! threads))))
(define with-mutex-locked
  (case-lambda
    ((mutex thunk) (with-mutex-locked mutex thunk #f))
    ((mutex thunk conditional-variable)
     (dynamic-wind
      (lambda () (mutex-lock! mutex))
      thunk
      (lambda () (mutex-unlock! mutex conditional-variable))))))
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (f)
      (lambda args (with-mutex-locked mutex (lambda () (apply f args)))))))