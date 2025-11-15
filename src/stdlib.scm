(define (cadr pair) 
    (car (cdr pair))
  )

(define (append a b)
  (if (null? a) b
      (cons (car a) (append (cdr a) b))))

(define map
  (lambda (f lst)
    (if (null? lst)
        '()
        (cons (f (car lst))
              (map f (cdr lst))))))

(define (list . xs) xs)


(define-macro (let bindings . body)
  `((lambda ,(map (lambda (b) (car b)) bindings)
      ,@body)
    ,@(map (lambda (b) (car (cdr b))) bindings)))


(define-macro (let* bindings . body)
  (if (null? bindings)
      `(let () ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings) ,@body))))


(define undefined 'undefined)

(define-macro (letrec bindings . body)
  (let ((vars (map car bindings)))
    `(let ,(map (lambda (v) (list v undefined)) vars)
       ,@(map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings)
       ,@body)))


(define-macro (cond . clauses)
  (if (null? clauses)
      (#f)
      (let ((cl (car clauses)))
        (if (eq? (car cl) 'else)
            `(begin ,@(cdr cl))
            `(if ,(car cl)
                 (begin ,@(cdr cl))
                 (cond ,@(cdr clauses)))))))


(define (reverse lst)
  (letrec ((reverse-iter
            (lambda (original reversed-so-far)
              (if (null? original)
                  reversed-so-far
                  (reverse-iter (cdr original) (cons (car original) reversed-so-far))))))
    (reverse-iter lst '())))


(define (equal? a b)
  (cond
    ((and (number? a) (number? b))
     (= a b))
    ((and (atom? a) (atom? b))
     (eq? a b))
    ((and (not (atom? a)) (not (atom? b)))
     (and (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))
    (else #f)))


(define-macro (not expr)
  `(if ,expr #f #t))

(define-macro (and . clauses)
  (if (null? clauses)
      #t
      (if (null? (cdr clauses))
          (car clauses)
          `(if ,(car clauses)
               (and ,@(cdr clauses))
               #f))))

(define-macro (or . clauses)
  (if (null? clauses)
      #f
      (if (null? (cdr clauses))
          (car clauses)
          `(let ((result ,(car clauses)))
             (if result
                 result
                 (or ,@(cdr clauses)))))))

