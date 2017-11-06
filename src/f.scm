(load "./utils/newLine.scm")

(define (first n)
    (if (< n 3)
        n
        (+ (first (- n 1)) (first (- n 2)) (first (- n 3)))))

(define (second n)
    (define (second-iter a b c count)
        (if (= count 0)
            c
            (second-iter (+ a b c) a b (- count 1))))
    (second-iter 2 1 0 n))

(newLine)
(write (first 17))
(newLine)
(write (second 17))
(newLine)
