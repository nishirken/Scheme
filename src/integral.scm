(load "utils/print.scm")
(load "utils/maths.scm")

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
        (*
            (sum f (+ a (/ dx 2)) add-dx b)
            dx))

(define (precise-integral f a b n)
    (define h
        (/ (- b a) n))
    (define (h-multiplier k)
        (cond
            ((= k 0) 1)
            ((= k n) 1)
            ((even? k) 2)
            (else 4)))
    (define (g k)
        (* (h-multiplier k) (f (+ a (* k h)))))
    (define (inc k)
        (+ k 1))
    (/ (* (sum g 0 inc n) h) 3))
