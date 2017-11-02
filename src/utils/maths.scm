(define (square x)
    (* x x))

(define (cubicSquare x)
    (* (square x) x))
    
(define (goodEnough? prevGuess guess)
    (< (abs (- prevGuess guess)) 0.000001))
