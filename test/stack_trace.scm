
(define (game-turn-A count)
  (display "Player A's turn. Count is: ") (display count) (newline)

  (if (= count 0)
      (error "Boom! Game over.")
      (game-turn-B (- count 1))))


(define (game-turn-B count)
  (display "Player B's turn. Count is: ") (display count) (newline)
  
  (game-turn-A (- count 1)))


(game-turn-B 3)
