(load "utils/print.scm")

(define (count-leaves x)
    (cond
        ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
        (count-leaves (cdr x))))))

; 2.24
; (list 1 (list 2 (list 3 4)))
;        /\
;       1  (list 2 (list 3 4))
;                 /\
;                2  (list 3 4)
;                        /\              
;                       3  4
; 
; (print (count-leaves (list 1 (list 2 (list 3 4))))) 4

; 2.25
; (define list-test (list 1 3 (list 5 7) 9)) (print (car (cdr (car (cdr (cdr list-test))))))
; (define list-test (list (list 7))) (print (car (car list-test)))
; (define list-test (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-test)))))))))))))

2.26
