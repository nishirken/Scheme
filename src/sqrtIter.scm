(load "utils/maths.scm")

(define (newIf predicate thenClause elseClause)
    (cond (predicate thenClause)
        (else elseClause)))

(define (newLine)
    (display "\n"))

(define (square x)
    (* x x))

(define (improve guess x)
    (average guess (/ x guess)))
(define (goodEnough? prevGuess guess)
    (< (abs (- prevGuess guess)) 0.000001))

(define (sqrtIter prevGuess guess x)
    (if (goodEnough? prevGuess guess)
        guess
        (sqrtIter guess (improve guess x) x)))

(define (sqrt x)
    (sqrtIter 0 1.0 x))
