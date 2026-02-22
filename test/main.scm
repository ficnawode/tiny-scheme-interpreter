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

(declare-test syntax-rules-basic
  (define-syntax id 
    (syntax-rules () 
      ((_ x) x)))
  (assert-eq 5 (id 5))
  (assert-eq 'sym (id 'sym))

  (define-syntax swap-list 
    (syntax-rules () 
      ((_ a b) (list b a))))
  (assert-equal? '(2 1) (swap-list 1 2))
  
  (define-syntax ignore-second
    (syntax-rules ()
      ((_ a b) a)))
  (assert-eq 10 (ignore-second 10 20))
)

(declare-test syntax-rules-literals
  (define-syntax what-is-it
    (syntax-rules (literal-a literal-b)
      ((_ literal-a) 'found-a)
      ((_ literal-b) 'found-b)
      ((_ other) 'found-other)))
  
  (assert-eq 'found-a (what-is-it literal-a))
  (assert-eq 'found-b (what-is-it literal-b))
  (assert-eq 'found-other (what-is-it literal-c))
  (assert-eq 'found-other (what-is-it something-else))

  (define-syntax my-cond-broken
    (syntax-rules ()
      ((_ else val) val)))
  (assert-eq 5 (my-cond-broken 1 5)) ;; Matches '1' as 'else'
  
  (define-syntax checks-literal
    (syntax-rules (hello)
      ((_ hello) 'yes)
      ((_ x) 'no)))
  (assert-eq 'yes (checks-literal hello))
  (assert-eq 'no (checks-literal hi))
)

(declare-test syntax-rules-ellipsis
  (define-syntax list* 
    (syntax-rules () 
      ((_ x ...) (list x ...))))
  
  (assert-equal? '(1 2 3) (list* 1 2 3))
  (assert-equal? '() (list*))
  
  (define-syntax required-plus-rest
    (syntax-rules ()
      ((_ first rest ...) (list first (list rest ...)))))
      
  (assert-equal? '(1 (2 3)) (required-plus-rest 1 2 3))
  (assert-equal? '(1 ()) (required-plus-rest 1))
)

(declare-test syntax-rules-ellipsis-advanced
  (define-syntax reverse-cons
    (syntax-rules ()
      ((_ head ... tail) (cons tail (list head ...)))))
      
  (assert-equal? '(4 1 2 3) (reverse-cons 1 2 3 4))

  (define-syntax zip
    (syntax-rules ()
      ((_ (k ...) (v ...)) 
       (list (list k v) ...))))
       
  (assert-equal? '((a 1) (b 2) (c 3)) (zip (a b c) (1 2 3)))
  
  (define-syntax deep-list
    (syntax-rules ()
      ((_ (x ...) ...)
       (list (list x ...) ...))))
  (assert-equal? '((1 2) (3 4)) (deep-list (1 2) (3 4)))
)

(declare-test syntax-rules-recursion
  (define-syntax my-append
    (syntax-rules ()
      ((_) '())
      ((_ l) l)
      ((_ l1 l2 rest ...)
       (append l1 (my-append l2 rest ...)))))
       
  (assert-equal? '(1 2 3 4 5 6) (my-append '(1 2) '(3 4) '(5 6)))
  
  (define-syntax my-let
    (syntax-rules ()
      ((_ ((x v) ...) body ...)
       ((lambda (x ...) body ...) v ...))))
       
  (assert-eq 30 (my-let ((x 10) (y 20)) (+ x y)))
)

(declare-test syntax-rules-wildcard
  (define-syntax check-wildcard
    (syntax-rules ()
      ((_ _ b) b)))
      
  (assert-eq 2 (check-wildcard 1 2))
  (assert-eq 2 (check-wildcard 'anything 2))
)

(declare-test syntax-rules-hygiene
  (define-syntax hygienic-swap
    (syntax-rules ()
      ((_ x y)
       (let ((temp x))
         (set! x y)
         (set! y temp)))))
         
  (let ((temp 5)
        (other 6))
    (hygienic-swap temp other)
    (assert-eq 6 temp)
    (assert-eq 5 other))

  (define-syntax use-list
    (syntax-rules ()
      ((_ x) (list x))))
      
  (let ((list +))
    (assert-equal? '(1) (use-list 1)))
)

(declare-test syntax-rules-errors
  
  (define-syntax only-one
    (syntax-rules ()
      ((_ x) x)))
      
  (assert-error? (only-one))
  (assert-error? (only-one 1 2))
  
  (define-syntax must-say-yes
    (syntax-rules (yes)
      ((_ yes) 'ok)))
      
  (assert-eq 'ok (must-say-yes yes))
  (assert-error? (must-say-yes no))
)

(declare-test number-tower-integers
  ;; Basic arithmetic
  (assert-eq 5 (+ 2 3))
  (assert-eq 1 (- 3 2))
  (assert-eq 6 (* 2 3))
  (assert-eq 4 (/ 8 2))
  
  ;; Negative numbers
  (assert-eq -5 (+ 2 -7))
  (assert-eq 6 (* -2 -3))
  (assert-eq -6 (* 2 -3))
  
  ;; Identity and Zero properties
  (assert-eq 5 (+ 5 0))
  (assert-eq 5 (* 5 1))
  (assert-eq 0 (* 5 0))
)

(declare-test number-tower-rationals
  ;; Creation via division of integers
  ;; 1/2 + 1/2 = 1
  (assert-eq 1 (+ (/ 1 2) (/ 1 2)))
  
  ;; 1/3 + 1/6 = 2/6 + 1/6 = 3/6 = 1/2
  (assert (= (/ 1 2) (+ (/ 1 3) (/ 1 6))))
  
  ;; Multiplication: 2/3 * 3/2 = 1
  (assert-eq 1 (* (/ 2 3) (/ 3 2)))
  
  ;; Division of rationals: (1/2) / (1/4) = 2
  (assert-eq 2 (/ (/ 1 2) (/ 1 4)))
  
  ;; Subtraction -> Negative Rationals
  ;; 1/2 - 1 = -1/2
  (assert (= (/ -1 2) (- (/ 1 2) 1)))
)

(declare-test number-tower-floats
  ;; Basic float arithmetic
  (assert (= 5.5 (+ 2.5 3.0)))
  (assert (= 1.5 (- 3.5 2.0)))
  (assert (= 6.25 (* 2.5 2.5)))
  
  ;; Float Division
  (assert (= 2.5 (/ 5.0 2.0)))
  
  ;; Mixed Float/Int arithmetic (Inexactness contagion)
  (assert (= 5.0 (+ 2 3.0)))
  (assert (= 2.5 (/ 5 2.0)))
)

(declare-test number-tower-mixed-types
  ;; Integer + Rational -> Rational
  ;; 2 + 1/2 = 5/2
  (assert (= (/ 5 2) (+ 2 (/ 1 2))))
  
  ;; Integer * Rational -> Integer (if divisible)
  ;; 4 * 1/2 = 2
  (assert-eq 2 (* 4 (/ 1 2)))
  
  ;; Rational + Float -> Float
  ;; 1/2 + 0.5 = 1.0
  (assert (= 1.0 (+ (/ 1 2) 0.5)))
  
  ;; Rational * Float -> Float
  (assert (= 0.25 (* (/ 1 2) 0.5)))
)

(declare-test number-tower-comparisons
  ;; Equality across types
  (assert (= 2 2.0))          ; Int = Float
  (assert (= 1 (/ 2 2)))      ; Int = Rational (simplified)
  (assert (= 0.5 (/ 1 2)))    ; Float = Rational
  
  ;; Inequality
  (assert (not (= 1 2)))
  (assert (not (= 1 1.000001)))
  
  ;; Less than / Greater than
  (assert (< 1 2))
  (assert (< 1 1.5))
  (assert (< (/ 1 2) 1))
  (assert (< (/ 1 3) (/ 1 2)))
  
  (assert (> 2 1))
  (assert (> 2.5 2))
  (assert (> (/ 1 2) (/ 1 3)))
  
  ;; Mixed comparisons
  (assert (> 1.0 (/ 1 2)))    ; 1.0 > 0.5
  (assert (< (/ 1 2) 0.6))    ; 0.5 < 0.6
)

(declare-test number-tower-error-conditions
  ;; Division by zero (Integers)
  (assert-error? (/ 1 0))
  
  ;; Division by zero (Rationals)
  (assert-error? (/ (/ 1 2) 0))
  
  ;; Type errors
  (assert-error? (+ 1 "2"))
  (assert-error? (= 1 "1"))
)

(declare-test number-tower-associativity
  ;; (a + b) + c = a + (b + c)
  (let ((a 1) (b 2) (c 3))
    (assert-eq (+ (+ a b) c) (+ a (+ b c))))
    
  ;; Mixed types associativity
  (let ((a 1) (b 2.5) (c (/ 1 2)))
    (assert (= (+ (+ a b) c) (+ a (+ b c)))))
)

(declare-test primitives-division-logic
  ;; Quotient (integer division)
  (assert-eq 3 (quotient 10 3))
  (assert-eq -3 (quotient -10 3))
  (assert-eq -3 (quotient 10 -3))
  (assert-eq 3 (quotient -10 -3))
  
  ;; Remainder (sign follows numerator)
  (assert-eq 1 (remainder 10 3))
  (assert-eq -1 (remainder -10 3))
  (assert-eq 1 (remainder 10 -3))
  (assert-eq -1 (remainder -10 -3))
  
  ;; Modulo (sign follows denominator)
  (assert-eq 1 (modulo 10 3))
  (assert-eq 2 (modulo -10 3))
  (assert-eq -2 (modulo 10 -3))
  (assert-eq -1 (modulo -10 -3))
  
  ;; Division by zero checks
  (assert-error? (quotient 10 0))
  (assert-error? (remainder 10 0))
  (assert-error? (modulo 10 0))
)

(declare-test primitives-exactness
  ;; Predicates
  (assert (exact? 5))
  (assert (inexact? 5.0))
  (assert (exact? (/ 1 2))) ;; Rationals are exact
  
  ;; Conversion
  (assert (inexact? (exact->inexact 5)))
  (assert-equal? 5.0 (exact->inexact 5))
  
  (assert (exact? (inexact->exact 5.0)))
  (assert-eq 5 (inexact->exact 5.0))
)

(declare-test stdlib-numeric-predicates
  (assert (zero? 0))
  (assert (zero? 0.0))
  (assert (not (zero? 1)))
  
  (assert (positive? 5))
  (assert (not (positive? -5)))
  (assert (not (positive? 0)))
  
  (assert (negative? -5))
  (assert (not (negative? 5)))
  (assert (not (negative? 0)))
  
  (assert (even? 4))
  (assert (even? -4))
  (assert (not (even? 3)))
  (assert (odd? 3))
  (assert (not (odd? 2)))
)

(declare-test stdlib-math-funcs
  ;; Abs
  (assert-eq 5 (abs 5))
  (assert-eq 5 (abs -5))
  (assert-eq 0 (abs 0))
  
  ;; Min/Max
  (assert-eq 5 (max 1 2 3 4 5))
  (assert-eq 5 (max 5 4 3 2 1))
  (assert-eq 1 (min 1 2 3 4 5))
  (assert-eq -5 (min 1 2 -5 4))
  
  ;; GCD
  (assert-eq 5 (gcd 10 5))
  (assert-eq 1 (gcd 7 3))
  (assert-eq 6 (gcd 12 18))
  (assert-eq 10 (gcd 0 10))
  
  ;; LCM
  (assert-eq 10 (lcm 2 5))
  (assert-eq 12 (lcm 4 6))
  (assert-eq 0 (lcm 0 5))
)
(declare-test number-tower-complex
  ;; Basic Creation and Parsing
  (assert-equal? 5i (make-rectangular 0 5))
  (assert-equal? 3+4i (make-rectangular 3 4))
  (assert-equal? -2-i (make-rectangular -2 -1))
  
  ;; Real/Imag Part Extraction
  (assert-eq 3 (real-part 3+4i))
  (assert-eq 4 (imag-part 3+4i))
  (assert-eq 0 (imag-part 5))
  
  ;; Canonicalization (3+0i should be the integer 3)
  (assert (exact? 3+0i))
  (assert (integer? 3+0i))
  (assert (not (complex? "not-a-num")))
  
  ;; Complex Arithmetic
  ;; (1+2i) + (3+4i) = 4+6i
  (assert-equal? 4+6i (+ 1+2i 3+4i))
  
  ;; (1+i) * (1+i) = 1 + i + i + i^2 = 1 + 2i - 1 = 0+2i = 2i
  (assert-equal? 2i (* 1+i 1+i))
  
  ;; (1+i) * (1-i) = 1^2 - i^2 = 1 - (-1) = 2
  (assert-eq 2 (* 1+i 1-i))
  
  ;; Complex Division
  ;; 1 / i = -i
  (assert-equal? -i (/ 1 0+1i))
  
  ;; (10+5i) / 5 = 2+i
  (assert-equal? 2+i (/ 10+5i 5))
  
  ;; Predicates
  (assert (complex? 3+4i))
  (assert (complex? 5)) ; R5RS: Reals are complex
  (assert (real? 5))
  (assert (not (real? 3+4i)))
  (assert (real? 3+0i))
)

(declare-test number-tower-complex-exactness
  ;; Exact + Inexact = Inexact
  (assert (inexact? (+ 1+1i 2.0)))
  (assert (inexact? (real-part (+ 1+1i 2.0))))
  
  ;; make-rectangular preserves exactness
  (assert (exact? (make-rectangular 1 2)))
  (assert (inexact? (make-rectangular 1.0 2)))
)

(declare-test cross-type-equality
  ;; Basic Equality
  (assert (= 1 1))
  (assert (= 1 1.0))
  (assert (not (= 1 0.5)))
  
  ;; Rational Equality
  (assert (= 1/2 0.5))
  (assert (= 1/2 2/4))
  (assert (not (= 1/3 0.33)))
  
  ;; Complex Canonicalization
  ;; 5+0i is real, 5+1i is not.
  (assert (= 5 5+0i))
  (assert (= 5.0 5+0i))
  (assert (not (= 5 5+1i)))
)

(declare-test cross-type-comparison
  ;; Integer comparisons
  (assert (< 1 2))
  (assert (not (< 1 1)))
  (assert (not (< 2 1)))
  
  ;; Float/Mixed
  (assert (< 1 1.5))
  (assert (> 1.5 1))
  (assert (= 1.0 1))

  ;; Rationals (Precision)
  ;; 1/3 is approx 0.3333333333333333
  (assert (< 1/3 0.4))
  (assert (> 0.4 1/3))
  (assert (< 0.3 1/3))
)

(declare-test complex-real-predicates
  ;; R5RS: Real numbers are complex numbers whose imaginary part is zero.
  (assert (complex? 5))
  (assert (real? 5))
  (assert (real? 5+0i))
  (assert (not (real? 5+1i)))
  
  ;; < and > return #f for non-real numbers
  (assert (not (< 1+i 2+i)))
  (assert (not (> 1+i 0+i)))
  
  ;; Real-part complex comparison
  (assert (< 1+0i 2+0i))
)

(declare-test rational-precision
  ;; (10 / 3) = 3.333...  vs (33 / 10) = 3.3
  (assert (> 10/3 33/10))
  (assert (not (= 10/3 33/10)))
)
(declare-test vectors-basic
  (assert (vector? (make-vector 5)))
  (assert (vector? #(1 2 3)))
  (assert (not (vector? '(1 2 3))))
  
  (assert-eq 3 (vector-length #(1 2 3)))
  (assert-eq 0 (vector-length #()))
)

(declare-test vectors-access
  (define v #(10 20 30))
  (assert-eq 10 (vector-ref v 0))
  (assert-eq 30 (vector-ref v 2))
  
  (vector-set! v 1 99)
  (assert-eq 99 (vector-ref v 1))
)

(declare-test vectors-conversion
  (define l '(a b c))
  (define v #(a b c))
  (assert-equal? l (vector->list v))
  (assert-equal? v (list->vector l))
)

(declare-test vectors-fill
  (define v (make-vector 3 'x))
  (assert-equal? #(x x x) v))

(run-all-tests)
