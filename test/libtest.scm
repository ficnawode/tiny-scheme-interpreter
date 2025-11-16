;; requires append and list (or implement append as above)
(define *test-manager*
  (let ((tests '()))
    (lambda (message . args)
      (cond
        ((eq? message 'add-test)
         (set! tests (append tests (list (car args)))))
        ((eq? message 'get-tests) tests)
        ((eq? message 'reset) (set! tests '()))))))

(define (register-test! name proc)
  (*test-manager* 'add-test (cons name proc)))

(define-macro (declare-test name . body)
  `(register-test!
    ',name
    (lambda ()
      (let ((all-passed?
              (and ,@body)))  
        all-passed?))))



(define-macro (assert expr)
  `(let ((result ,expr))
     (if result
         #t
         (begin
           (display "  -> Assertion Failed: ") (display ',expr) (newline)
           #f))))

(define-macro (assert-eq expr1 expr2)
  `(assert (= ,expr1 ,expr2)))

(define-macro (assert-eq? expr1 expr2)
  `(assert (eq? ,expr1 ,expr2)))

(define-macro (assert-equal? expr1 expr2)
  `(assert (equal? ,expr1 ,expr2)))

(define-macro (assert-error? expr1 )
  `(assert (error-object? ,expr1)))

(define-macro (assert-no-error? expr1 )
  `(assert (not (error-object? ,expr1))))

(define (run-all-tests)
  (let ((tests (*test-manager* 'get-tests)))
    (display "====================") (newline)
    (display "Running All Tests...") (newline)
    (display "====================") (newline)
    (letrec ((loop (lambda (tests-to-run passed failed)
                     (if (null? tests-to-run)
                         (begin
                           (display "====================") (newline)
                           (display "TEST SUMMARY") (newline)
                           (display "  Total Tests:  ") (display (+ passed failed)) (newline)
                           (display "  Passed:       ") (display passed) (newline)
                           (display "  Failed:       ") (display failed) (newline)
                           (display "====================") (newline))
                         (let* ((current-test (car tests-to-run))
                                (test-name (car current-test))
                                (test-proc (cdr current-test)))
                           (display "[ RUN      ] ") (display test-name) (newline)
                           (if (test-proc)
                               (begin
                                 (display "[       OK ] ") (display test-name) (newline) (newline)
                                 (loop (cdr tests-to-run) (+ passed 1) failed))
                               (begin
                                 (display "[   FAILED ] ") (display test-name) (newline) (newline)
                                 (loop (cdr tests-to-run) passed (+ failed 1)))))))))
      (loop tests 0 0))))
