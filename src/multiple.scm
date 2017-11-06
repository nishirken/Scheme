(load "utils/print.scm")
(load "utils/maths.scm")

(define (multiple a b)
    (if (= b 0)
        0
        (+ a (multiple a (- b 1)))))

(define (double x)
    (if (= x 0)
        0
        (+ x x)))

(define (halve x)
    (/ x 2))

(define (fast-multiple a b)
    (cond
        ((= a 0) 0)
        ((even? a) (fast-multiple (halve a) (double b)))
        ((pos? a) (+ (fast-multiple (halve (dec a)) (double b)) b))
        (else (- (fast-multiple (halve (inc a)) (double b)) b))))

(print (remainder 13 7))


