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
(define (filter list-instance f)
    (let ((limit (dec (length list-instance))))
        (define (iter counter result)
            (let 
                ((current-item (list-ref list-instance counter)))
                (cond
                    ((= counter limit)
                        (if (f current-item)
                            (append result (list current-item))
                            result))
                    ((f current-item) (iter (inc counter) (append result (list current-item))))
                    (else (iter (inc counter) result)))))
        (iter 0 (list))))

(define (same-parity . x)
    (let ((even-or-odd
        (if (even? (list-ref x 0))
            even?
            odd?)))
        (filter x (lambda (current-item) (even-or-odd current-item)))))

; (print (same-parity 1 2 3 4 7 8 9 11))
