
(display "Testing begin special form...")
(newline)
(if #t
  (begin 
    (display "hello, ")
    (display "begin! ")
    (newline)
    3
    )
  (display "This shouldn't be here"))




(display "Testing let bindings...")
(newline)

(let ((a 5)
      (b 10))
  (begin
    (display "a = ")
    (display a)
    (newline)

    (display "b = ")
    (display b)
    (newline)

    (display "a + b = ")
    (display (+ a b))
    (newline)))

(define c 99)
(let ((c 15))
  (display "Local c inside let is ")
  (display c)
  (newline))

(display "Global c outside let is still ")
(display c)
(newline)





(display "--- Testing letrec ---")
(newline)

(letrec ((fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1)))))))
  (display "Factorial of 6 is: ")
  (display (fact 6))
  (newline))

;; A classic mutual recursion test
(letrec ((is-even? (lambda (n)
                     (if (= n 0) #t (is-odd? (- n 1)))))
         (is-odd? (lambda (n)
                    (if (= n 0) () (is-even? (- n 1))))))
  (display "Is 10 even? ")
  (display (is-even? 10))
  (newline)
  (display "Is 11 odd? ")
  (display (is-odd? 11))
  (newline))

(display "Testing cond macro...")
(newline)

(display "Test 1: Should print 'First clause matched'")
(newline)
(cond
  ((= 10 10) (display "-> OK: First clause matched") (newline))
  ((= 10 5)  (display "-> FAIL: Second clause matched") (newline))
  (else      (display "-> FAIL: Else clause matched") (newline)))
(newline)


(display "Test 2: Should print 'Second clause matched'")
(newline)
(cond
  ((= 10 5)  (display "-> FAIL: First clause matched") (newline))
  ((> 10 5)  (display "-> OK: Second clause matched") (newline))
  (else      (display "-> FAIL: Else clause matched") (newline)))
(newline)


(display "Test 3: Should print 'Else clause matched'")
(newline)
(cond
  ((null? '(a)) (display "-> FAIL: First clause matched") (newline))
  ((string? 10) (display "-> FAIL: Second clause matched") (newline))
  (else         (display "-> OK: Else clause matched") (newline)))
(newline)


(display "Test 4: The return value should be 42")
(newline)
(let ((result (cond
                ((= 1 2) 10)
                ((= 2 2) (display "-> OK: Correct clause chosen") (newline) 42)
                (else 30))))
  (display "-> Result is: ")
  (display result)
  (newline))
(newline)


(display "Test 5: No match should return ()")
(newline)
(display "-> Result is: ")
(cond
  ((= 1 2) 100)
  ((= 1 3) 200))
(newline) 


(display "Test 6: Testing truthiness of various values")
(newline)
(cond
  ('()       (display "-> FAIL: () was true") (newline))
  (0         (display "-> OK: 0 is true") (newline))
  (else      (display "-> FAIL: Else was reached") (newline)))

(cond
  ('()       (display "-> FAIL: () was true") (newline))
  ("#f"      (display "-> OK: The string \"#f\" is true") (newline))
  (else      (display "-> FAIL: Else was reached") (newline)))

(cond
  ((cons 1 2) (display "-> OK: A pair is true") (newline))
  (else       (display "-> FAIL: Else was reached") (newline)))

(display "Testing logical macros...")
(newline)

(display "not #t -> ")(display (not #t)) (newline)
(display "not () -> ")(display (not '())) (newline)
(display "not 0  -> ")(display (not 0)) (newline)
(newline)
