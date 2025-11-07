
(define-macro (begin expr)
  ((lambda (body)
     `((lambda () ,@body)))
   (cdr expr)))

(if #t
  (begin 
    (display "hello, ")
    (display "begin! ")
    (newline)
    3
    )
  ())
