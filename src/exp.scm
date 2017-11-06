(load "utils/print.scm")
(load "sqrtIter.scm")

(define (exp base degree)
    (define (exp-iter base counter state)
        (cond
            ((= counter 0) state)
            ((even? counter) (exp-iter (square base) (/ counter 2) state))
            (else (exp-iter base (- counter 1) (* state base)))))
        (exp-iter base degree 1))

(print (exp 3 3))
