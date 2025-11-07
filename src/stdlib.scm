(define (cadr pair) 
    (car (cdr pair))
  )

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))
      )
  )

(define-macro (let bindings . body)
  `((lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
  )

