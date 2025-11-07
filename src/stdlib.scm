(define-macro (begin expr)
  ((lambda (body)
     `((lambda () ,@body)))
   (cdr expr)))

(define (map f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))
    )
  )


