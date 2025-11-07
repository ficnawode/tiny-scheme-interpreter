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
