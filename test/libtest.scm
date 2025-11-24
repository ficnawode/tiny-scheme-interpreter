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

(define-syntax declare-test
  (syntax-rules ()
    ((_ name body ...)
     (register-test! 
      'name 
      (lambda ()
        (let ((all-passed? (and body ...)))
          all-passed?))))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (let ((result expr))
       (if result
           #t
           (begin
             (display "  -> Assertion Failed: ") 
             (display 'expr) 
             (newline)
             #f))))))

(define-syntax assert-eq
  (syntax-rules ()
    ((_ e1 e2)
     (assert (= e1 e2)))))

(define-syntax assert-eq?
  (syntax-rules ()
    ((_ e1 e2)
     (assert (eq? e1 e2)))))

(define-syntax assert-equal?
  (syntax-rules ()
    ((_ e1 e2)
     (assert (equal? e1 e2)))))

(define-syntax assert-error?
  (syntax-rules ()
    ((_ expr)
     (assert (error-object? expr)))))

(define-syntax assert-no-error?
  (syntax-rules ()
    ((_ expr)
     (assert (not (error-object? expr))))))

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
