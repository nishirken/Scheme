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
    (accumulate adjoin-set-multi set2 set1))

(define (intersection-set-multi set1 set2)
    (intersection-set set1 set2))

; 2.61
(define (element-of-set-sort? x set)
    (cond
        ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-sort? x (cdr set)))))

(define (intersection-set-sort set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond
                ((= x1 x2)
                    (cons x1
                        (intersection-set-sort (cdr set1)
                            (cdr set2))))
                ((< x1 x2)
                    (intersection-set-sort (cdr set1) set2))
                ((< x2 x1)
                    (intersection-set-sort set1 (cdr set2)))))))
    
(define (adjoin-set-sort x set)
    (if (element-of-set-sort?)
        set
        (cons x set)))

; 2.62
(define (union-set-sort set1 set2)
    (accumulate adjoin-set-sort set2 set1)) ; (n/2 + n)
