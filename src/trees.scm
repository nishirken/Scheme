(load "utils/print.scm")
(load "utils/maths.scm")

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

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

; (print (append x y))
; (1 2 3 4 5 6)
; (print (cons x y))
; ((1 2 3) 4 5 6)
; (print (list x y))
; ((1 2 3) (4 5 6))

; 2.27
; TODO
; (define (reverse list-instance)
;     (let ((list-length (length list-instance)))
;         (define (iter counter result)
;             (if (= counter 0)
;                 result
;                 (iter (dec counter) (append result (list (list-ref list-instance (dec counter)))))))
;         (iter list-length (list))))

; (define (deep-reverse seq)
;     ())

; (print (deep-reverse (list 1 (list 3 6 1) 6)))

; 2.28
