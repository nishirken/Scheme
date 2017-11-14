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
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount count-list
                     (- kinds-of-coins 1))
                 (cc (- amount
                    (list-ref count-list kinds-of-coins)) count-list kinds-of-coins)))))

(define (count-change amount count-list)
    (cc amount count-list (dec (length count-list))))

(define us-coins (list 50 25 10 5 1))
; (print (count-change 12 us-coins))
