(load "utils/print.scm")

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

; 1.35 The golden ratio
; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; 1.36 x pow x
; (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.0)

; 1.37 continued fraction
(define (cont-frac n d k)
    (define (iter counter result)
        (if (= counter 0)
            result
            (iter (- counter 1) (/ (n counter) (+ (d counter) result)))))
    (iter k 0))

; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
