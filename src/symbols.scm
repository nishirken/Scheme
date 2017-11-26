(load "utils/print.scm")

; 2.53
(define (memq item x)
    (cond ((null? x) false)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))

; (print (list 'a 'b 'c))
; (print (list 'george))
; (print (cdr '((x1 x2) (y1 y2))))
; (print (cadr '((x1 x2) (y1 y2))))
; (print (pair? (car '(a short list))))
; (print (memq 'red '((red shoes) (blue socks))))
; (print (memq 'red '(red shoes blue socks)))

; 2.54
(define (equal-custom? first second)
    (cond
        ((and (number? first) (number? second)) (= first second))
        ((and (symbol? first) (symbol? second)) (eq? first second))
        ((and (list? first) (list? second))
            (and (equal-custom? (car first) (car second))
                (equal-custom? (cadr first) (cadr second))))
        ((and (pair? first) (pair? second))
            (and (eq? (car first) (car second)) (eq? (cdr first) (cdr second))))
        (else #f)))

; (print (equal-custom? '(a b a b) '(a b a b)))

; 2.55
; (print (quote 'abracadabra))
