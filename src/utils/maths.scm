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
