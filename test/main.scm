(load "test/libtest.scm")

(declare-test primitives-arithmetic
  (assert-eq 10 (+ 1 2 3 4) )
  (assert-eq 0 (+ 5 -5))
  (assert-eq 3 (- 10 7))
  (assert-eq -5 (- 5)) 
  (assert-eq 30 (* 2 3 5))
  (assert-eq 1 (* 1))   ;
  (assert-eq 5 (/ 10 2))
  (assert-eq 2 (/ 20 5 2)))

(declare-test primitives-numeric-predicates
  (assert (= 5 5))
  (assert (not (= 5 6)))
  (assert (> 10 5))
  (assert (not (> 5 10)))
  (assert (< 3 8))
  (assert (not (< 8 3))))

 (declare-test primitives-list-operations
   (assert-eq? 'a (car (cons 'a 'b)))
   (assert-eq? 'b (cdr (cons 'a 'b)))
   (assert-eq? 'b (car (cdr '(a b c))))
   )

(declare-test primitives-type-predicates
  (assert (null? '()))
  (assert (not (null? '(a))))
  (assert (atom? 'a))
  (assert (atom? 123))
  (assert (atom? '()))
  (assert (not (atom? '(a b))))
  (assert (string? "hello"))
  (assert (not (string? 'hello)))
  (assert (not (null? #f)))
  (assert (eq? #f #f))
)

(declare-test primitives-equality
  (assert (eq? 'a 'a))
  (assert (not (eq? 'a 'b)))
  (assert (eq? #t #t))
  (assert (not (eq? '(1 2) '(1 2))))
  (assert (not (eq? #f '())))
  )

(declare-test primitives-list-operations
   (assert-eq? 'a (car (cons 'a 'b)))
   (assert-eq? 'b (cdr (cons 'a 'b)))
   (assert-eq? 'b (car (cdr '(a b c))))

   (assert-eq 3 (length '(a b c)))
   (assert-eq 0 (length '()))
   (assert-eq 1 (length '(1)))
   (assert-eq 4 (length '(1 "two" (3 4) #t))) 

   (assert-equal? '(1 2 3 4) (append '(1 2) '(3 4)))
   (assert-equal? '(a b c d e) (append '(a) '(b c) '(d e)))
   (assert-equal? '(1 2) (append '() '(1 2))) 
   (assert-equal? '(1 2) (append '(1 2) '())) 
   (assert-equal? '() (append '() '()))
   (assert-equal? '() (append)) 

   (assert-equal? '(1 2 3 . 4) (append '(1 2) '(3 . 4)))
   (assert-equal? '(1 2 . 3) (append '(1 2) 3))

   (define single-arg-list '(a b c))
   (assert (equal? single-arg-list (append single-arg-list)))
   )

(declare-test scope-lexical-closures
  (define (make-adder num-to-add)
    (lambda (x) (+ x num-to-add)))

  (define add-5 (make-adder 5))
  (define add-10 (make-adder 10))

  (assert-equal? 8 (add-5 3))
  (assert-equal? 13 (add-10 3))
  (assert-equal? 9 (add-5 4)))

(declare-test mutation-set-bang
  (define set-var 1)
  (set! set-var 5)
  (assert-equal? 5 set-var)

  (define counter 0)
  (define (increment!) (set! counter (+ counter 1)))
  (increment!)
  (increment!)
  (assert-equal? 2 counter))

(declare-test data-type-improper-lists
  (define my-pair (cons 1 2))
  (assert-eq 1 (car my-pair))
  (assert-eq 2 (cdr my-pair))
  (assert (not (list? my-pair))) 
)

(declare-test special-form-if
  (assert-eq 1 (if #t 1 2))
  (assert-eq 1 (if 'true 1 2))
  (assert-eq 1 (if 0 1 2))
  (assert-eq 1 (if "hello" 1 2))
  (assert-eq 1 (if '() 1 2))
  (assert-eq 2 (if #f 1 2))
  )

(declare-test special-form-lambda-and-application
  (assert-eq 20 ((lambda (x) (* x 2)) 10))
  (assert-eq 7 ((lambda (x y) (+ x y)) 3 4))
  (assert-equal? 'b ((lambda (a b c) b) 'a 'b 'c)))

(declare-test special-form-define
  (define test-var 100)
  (assert-eq 100 test-var)

  (define (square x) (* x x))
  (assert-eq 25 (square 5))
  (assert-eq 100 (square 10))
  )

(declare-test special-form-begin
  (define begin-var 0)
  (assert-eq 3 (begin
                     (set! begin-var 1)
                     (set! begin-var 2)
                     (+ begin-var 1))))

(declare-test special-form-apply
  (assert-eq 10 (apply + '(1 2 3 4)))
  (assert-equal? '(2 3 4) (apply cdr '((1 2 3 4)))))

(declare-test advanced-quasiquote
  (let* ((b 2)
         (c-list '(3 4)))
    (assert-equal? '(1 2 (3 4)) `(1 ,b ,c-list))
    (assert-equal? '(1 2 3 4 5) `(1 ,b ,@c-list 5))))

(declare-test advanced-gensym
  (assert (not (eq? (gensym) (gensym))))
  (assert (not (eq? (gensym "G") (gensym "G")))))

(declare-test advanced-strings
  (assert-equal? 5 (string-length "hello"))
  (assert-equal? 0 (string-length ""))
  (assert-equal? 11 (string-length (string-append "hello" " " "world"))))

(declare-test stdlib-macro-let
  (let ((a 3))
  (assert-eq 3 (let ((a 2) (b a)) b))))

(declare-test stdlib-macro-let-star
  (assert-eq 3
    (let* ((x 1)
           (y (+ x 1))) 
      (+ x y))))

(declare-test stdlib-function-reverse
  (assert-equal? '(3 2 1) (reverse '(1 2 3))) 
  (assert-equal? '(c b a) (reverse '(a b c)))
  (assert-equal? '(a) (reverse '(a)))
  (assert-equal? '() (reverse '()))
  (assert (eq? (car (reverse '(a b))) 'b)))


(declare-test stdlib-cond
  (assert-equal? 'greater (cond ((> 3 2) 'greater)
                                ((< 3 2) 'less)))
  (assert-equal? 'equal (cond ((> 3 4) 'greater)
                              ((= 3 3) 'equal)))
  (assert-equal? 'else-clause (cond ((< 1 0) 1)
                                    ((< 2 1) 2)
                                    (else 'else-clause)))
  (assert-equal? #f (cond))
  (assert-equal? 42 (cond (42)))
  (assert-equal? #f (cond (#f)))
  (assert-equal? 'b (cond ((assoc 'y '((x a) (y b))) => cadr) (else #f)))
  (assert-equal? #f (cond ((assoc 'z '((x a) (y b))) => cadr)(else #f)))

  (assert-no-error? (cond))
  (assert-error? (cond (else) ))
  (assert-error?  (cond (else 1) (2)))
  (assert-error?  (cond (test =>)))
  (assert-error?  (cond (test => proc extra)))
  (assert-error? (cond 'not-a-list))
)

(declare-test stdlib-boolean-ops
  (assert (not #f))
  (assert (not (not #t)))
  (assert (and #t #t))
  (assert (not (and #t #f)))
  (assert (or #t #f))
  (assert (not (or #f #f)))
  (assert (equal? #t #t))
  (assert (not (equal? #t #f)))

  (assert (and #t #t '() #t))
  (assert (not (and #t #t #f #t)))

  (assert (or #f '()) )
  (assert-equal? 'is-true (or #f #f 'is-true 'another-true))
  (assert-equal? #f (or #f #f #f))
)

(declare-test runtime-errors
  (assert-no-error? 1)

  (assert-error? (define 1))
  (assert-error? (load))
  (assert-error? (load 1))
  (assert-error? (set! 1))
  (assert-error? (set! unbound-symbol))
  (assert-error? (set! unbound-symbol 1))
  (assert-error? (set! unbound-symbol 1 2 3 ))
  (assert-error? (apply ()))
  (assert-error? (apply () 1))
  (assert-error? (1 2)) ; attempt to call non-function
  (assert-error? ,@1 )

  (define-macro (deep-macro n)
    (if (= n 0)
      0  
      `(deep-macro ,(- n 1)))) 
  (assert-no-error?(deep-macro 98))
  (assert-eq 0 (deep-macro 98))
  (assert-error?(deep-macro 99))

)

(declare-test primitive-errors
  (assert-error? (+ 1 "two"))
  (assert-error? (+ 1 2 3 "four"))
  (assert-error? (- 5 "two"))
  (assert-error? (- "five" 2))
  (assert-error? (* 5 "two"))
  (assert-error? (/ 10 "two"))
  (assert-error? (/))
  (assert-error? (/ 10))

  (assert-error? (= 1))
  (assert-error? (= 1 2 3))
  (assert-error? (= 1 "cat"))
  (assert-error? (< 1))
  (assert-error? (< 1 2 3))
  (assert-error? (< 1 "2"))
  (assert-error? (> 1))
  (assert-error? (> 1 2 3))
  (assert-error? (> "1" 2))

  (assert-error? (cons 1))
  (assert-error? (cons 1 2 3))
  (assert-error? (car))
  (assert-error? (car '(1) '(2)))
  (assert-error? (car 1))
  (assert-error? (car "string"))
  (assert-error? (cdr))
  (assert-error? (cdr '(1) '(2)))
  (assert-error? (cdr 1))
  (assert-error? (cdr "string"))

  (assert-error? (number?))
  (assert-error? (number? 1 2))
  (assert-error? (list?))
  (assert-error? (list? '(1) '(2)))
  (assert-error? (eq? 1))
  (assert-error? (eq? 1 2 3))
  (assert-error? (atom?))
  (assert-error? (atom? 1 2))
  (assert-error? (null?))
  (assert-error? (null? '() '()))
  (assert-error? (string?))
  (assert-error? (string? "a" "b"))
  (assert-error? (error-object?))
  (assert-error? (error-object? (error "foo") (error "bar")))

  (assert-error? (string-length))
  (assert-error? (string-length "a" "b"))
  (assert-error? (string-length 123))
  (assert-error? (string-append "hello" 123))
  (assert-error? (string-append 123 "world"))

  (assert-error? (gensym 123))
  (assert-error? (gensym "prefix" "extra"))
  (assert-error? (error 123))
  (assert-error? (error))
)

(run-all-tests)
