(load "utils/print.scm")

(define (sin angle)
    (if (< (abs angle) 0.1)
        angle
        (p (sin (/ angle 3.0)))))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(print (p (- 0.927 0)))
