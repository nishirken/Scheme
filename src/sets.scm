(load "utils/print.scm")

(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond
        ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
            (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59
(define (union-set set1 set2)
    (reduce (lambda (x y) (adjoin-set x y)) set1 set2))

(print (union-set '(1 2 3) '(a b c)))
