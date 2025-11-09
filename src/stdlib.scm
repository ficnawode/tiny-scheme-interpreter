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

(define-macro (letrec bindings . body)
  (let ((vars (map car bindings))
        (vals (map cadr bindings)))

    ; some serious hackery here: a lambda takes itself as a parameter
    (let ((make-setters-inner
            (lambda (self vs ls)
              (if (null? vs)
                  '()
                  (cons `(set! ,(car vs) ,(car ls))
                        (self self (cdr vs) (cdr ls)))))))

      `(let ,(map (lambda (v) `(,v '())) vars)
         ,@(make-setters-inner make-setters-inner vars vals)
         ,@body))))

(define-macro (cond . clauses)
  (letrec
      ((expand-clauses (lambda (cls)
         (if (null? cls)
             '()
             (let ((first-clause (car cls))
                   (rest-clauses (cdr cls)))
               (let ((test (car first-clause)))
                 (if (eq? test 'else)
                     `(begin ,@(cdr first-clause))
                     `(if ,test
                          (begin ,@(cdr first-clause))
                          ,(expand-clauses rest-clauses)))))))))
    (expand-clauses clauses)))


(define-macro (not expr)
  `(if ,expr '() #t))
