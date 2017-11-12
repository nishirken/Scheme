(define (square x)
    (* x x))

(define (cube x)
    (* (square x) x))

(define (factorial x)
    (define (fact-iter product counter max-count)
        (if (> counter max-count)
            product
            (fact-iter (* counter product) (+ counter 1) max-count)))
    (fact-iter 1 1 x))

(define (dec x)
    (- x 1))

(define (inc x)
    (+ x 1))

(define (pos? x)
    (> x 0))

(define (neg? x)
    (< 0 x))

(define (average x y)
    (/ (+ x y) 2))
