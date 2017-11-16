(load "utils/print.scm")
(load "utils/maths.scm")

(define simple-list
    (list 1 2 3 4 "last"))

; 2.17
(define (last-pair list-instance)
    (let ((list-length (length list-instance)))
        (list-ref list-instance (dec list-length))))

; (print (last-pair simple-list)

; 2.18
(define (reverse list-instance)
    (let ((list-length (length list-instance)))
        (define (iter counter result)
            (if (= counter 0)
                result
                (iter (dec counter) (append result (list (list-ref list-instance (dec counter)))))))
        (iter list-length (list))))

; (print (reverse simple-list))

; 2.19
(define (cc amount count-list kinds-of-coins)
    (cond
        ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins (- 1))) 0)
        (else (+ (cc amount count-list
                     (dec kinds-of-coins))
                 (cc (- amount
                    (list-ref count-list kinds-of-coins)) count-list kinds-of-coins)))))

(define (count-change amount count-list)
    (cc amount count-list (dec (length count-list))))

(define us-coins (list 50 25 10 5 1))
; (print (count-change 100 us-coins))

; 2.20
(define (not-null-execute items f)
    (if (null? items)
        '()
        (f items)))

(define (filter items f)
    (not-null-execute items (lambda (x)
        (if (f (car items))
            (cons (car items) (filter (cdr items) f))
            (filter (cdr items) f)))))
        
(define (same-parity . x)
    (let ((even-or-odd
        (if (even? (list-ref x 0))
            even?
            odd?)))
        (filter x (lambda (current-item) (even-or-odd current-item)))))

; (print (same-parity 1 2 3 4 7 8 9 11))

; 2.21
(define (map items f)
    (not-null-execute items (lambda (x)
        '()
        (cons (f (car items))
            (map (cdr items) f)))))

(define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
    (map items square))

; (print (square-list-map (list 1 2 3 4)))

; 2.22
(define (square-list-iter items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                (append answer (list (square (car things)))))))
    (iter items (list)))

; (print (square-list-iter (list 1 3 9 12)))

; 2.23
(define (for-each items f)
    (not-null-execute items (lambda (items)
        (begin (f (car items))
        (for-each (cdr items) f)))))

; (print (for-each (list 1 4 5) (lambda (item) (display item))))

; TODO: reduce
