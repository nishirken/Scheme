(load "utils/maths.scm")

(define (create-iter f)
    (define (iter counter result)
        (if (= counter 0)
            result
            (iter (dec counter) (f result))))
    iter)
