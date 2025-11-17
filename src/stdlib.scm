(define (cadr pair) 
    (car (cdr pair))
  )

(define (caar pair) 
    (car (car pair))
  )
(define (cddr pair) 
    (cdr (cdr pair))
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
      #f  ; Unspecified in R7RS
      (let ((clause (car clauses))
            (rest-clauses (cdr clauses)))
        (if (not (list? clause))
            (error "cond: invalid clause syntax")
            (let ((test (car clause))
                  (clause-rest (cdr clause)))
              (if (eq? test 'else)
                  (if (not (null? rest-clauses))
                      (error "cond: else must be the last clause")
                      (if (null? clause-rest)
                          (error "cond: else clause requires expressions")
                          `(begin ,@clause-rest)))
                  (if (null? clause-rest)
                      `(or ,test (cond ,@rest-clauses))
                      (if (eq? (car clause-rest) '=>)
                          (if (null? (cdr clause-rest))
                              (error "cond: => requires a procedure")
                              (if (not (null? (cddr clause-rest)))
                                  (error "cond: too many expressions after =>")
                                  `(let ((tmp ,test))
                                     (if tmp
                                         (,(cadr clause-rest) tmp)
                                         (cond ,@rest-clauses)))))
                          `(if ,test
                               (begin ,@clause-rest)
                               (cond ,@rest-clauses))))))))))
(define (assoc key alist)
  (cond ((null? alist) #f)
  ((eq? key (caar alist)) (car alist))
  (else (assoc key (cdr alist)))))

(define (reverse lst)
  (letrec ((reverse-iter
            (lambda (original reversed-so-far)
              (if (null? original)
                  reversed-so-far
                  (reverse-iter (cdr original) (cons (car original) reversed-so-far))))))
    (reverse-iter lst '())))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append . lists)
  (letrec ((append2
            (lambda (lst1 lst2)
              (if (null? lst1)
                  lst2
                  (cons (car lst1)
                        (append2 (cdr lst1) lst2))))))
    (cond
      ((null? lists) '())
      ((null? (cdr lists)) (car lists))
      (else (append2 (car lists) (apply append (cdr lists)))))))

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

