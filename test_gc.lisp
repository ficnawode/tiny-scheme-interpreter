(define (create-garbage-recursive i useless-arg)
  (if (= i 0)
      #t 
      (create-garbage-recursive (- i 1) (cons i i))))

(define (create-garbage n)
  (create-garbage-recursive n '()))

(newline)
(create-garbage 1000)
(newline)
