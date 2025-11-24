(define (cadr pair) 
    (car (cdr pair))
  )

(define (caar pair) 
    (car (car pair))
  )

(define (cddr pair) 
    (cdr (cdr pair))
  )

(define map
  (lambda (f lst)
    (if (null? lst)
        '()
        (cons (f (car lst))
              (map f (cdr lst))))))

(define (list . xs) xs)

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



(define undefined 'undefined)

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    
    ((let tag ((name val) ...) body1 body2 ...)
     (letrec ((tag (lambda (name ...) body1 body2 ...)))
       (tag val ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var init) ...) body1 body2 ...)
     (let ((var undefined) ...)
       (set! var init)
       ...
       (let () body1 body2 ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax not
  (syntax-rules ()
    ((_ expression)
     (if expression #f #t))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond) #f)

    ((cond (else result1 result2 ...) clause1 . rest-clauses)
     (error "cond: else must be the last clause"))

    ((cond (else))
     (error "cond: else clause requires expressions"))

    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))

    ((cond (test =>))
     (error "cond: => requires a procedure"))

    ((cond (test =>) clause1 . rest-clauses)
     (error "cond: => requires a procedure"))

    ((cond (test => result extra . extras) . rest-clauses)
     (error "cond: too many expressions after =>"))

    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp) #f)))

    ((cond (test => result) clause1 . rest-clauses)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 . rest-clauses))))

    ((cond (test))
     (let ((temp test))
       (if temp temp #f)))

    ((cond (test) clause1 . rest-clauses)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 . rest-clauses))))

    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...) #f))

    ((cond (test result1 result2 ...) clause1 . rest-clauses)
     (if test
         (begin result1 result2 ...)
         (cond clause1 . rest-clauses)))))


