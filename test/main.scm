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


(declare-test stdlib-cond-and-or
  (assert-equal? 'greater (cond ((> 3 2) 'greater)
                            ((< 3 2) 'less)))
  (assert-equal? 'equal (cond ((> 3 4) 'greater)
                          ((= 3 3) 'equal)))
  (assert-equal? 'else-clause (cond ((< 1 0) 1)
                                ((< 2 1) 2)
                                (else 'else-clause)))
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

(run-all-tests)
