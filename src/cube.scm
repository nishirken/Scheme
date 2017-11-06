(define (square x)
    (* x x))

(define (improve guess x)
    (/
        (+ (/ x (square guess)) (* 2 guess))
        3
    )
)

(define (goodEnough? prevGuess guess)
  (< (abs (- guess prevGuess))
     0.00001))

(define (cubicSqrtIter prevGuess guess x)
    (if (goodEnough? prevGuess guess)
        guess
        (cubicSqrtIter guess (improve guess x) x)
    )
)

(define (cubicSqrt x)
    (cubicSqrtIter 0 1.0 x))

(display "\n")
(display (cubicSqrt 27.0))
(display "\n")