(define (sos x y)
  (+ (sq x) (sq y))
  )

(define (sq x)
  (* x x)
  )

(display (sos 3 4))
(newline)
