(load "utils/print.scm")
(load "trees.scm")

(define (element-of-set? x set)
    (cond
        ((null? set) #f)
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
    (accumulate adjoin-set set2 set1))

; (print (union-set '(1 2 3) '(a b c)))

; 2.60
(define sample-set '(2 3 2 1 3 2 2))

(define (element-of-set-multi? x set)
    (element-of-set? x set))

(define (adjoin-set-multi x set)
    (cons x set))

(define (union-set-multi set1 set2)
    (accumulate adjoin-set set2 set1))

(define (intersection-set-multi set1 set2)
    (intersection-set-multi set1 set2))
