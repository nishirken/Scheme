(load "utils/print.scm")
(load "utils/maths.scm")

; 1.41 double
(define (double f)
    (lambda (x) (f (f x))))

; (print (double square) 2))

; 1.42 compose
(define (compose f g)
    (lambda (x) (f (g x))))

; (print ((compose inc square) 2))

; 1.43 repeated
(define (repeated f n)
    (define (iter counter result)
        (if (= counter 0)
            result
            (iter (- counter 1) (compose f result))))
    (iter (- n 1) f))

; ((repeated square 2) 5)

; 1.44 smooth
(define (smooth f dx)
    (lambda (x)
        (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 2)))

(define (smooth-repeated f dx n)
    ((repeated
        (lambda (g)
            (smooth g dx)) n) f))

; 1.45 iterative-improve
(define (iterative-improve fn-enough? fn-improve)
    (define (iter result)
        (if (fn-enough? result)
            result
            (iter (fn-improve result))))
    (lambda (x) (iter x)))
