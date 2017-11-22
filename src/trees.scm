(load "utils/print.scm")
(load "utils/maths.scm")
(load "compose.scm")

(define (atom? x)
    (and (not (null? x))
        (not (pair? x))))

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
(define (deep-reverse seq)
    (if (list? seq)
        (map deep-reverse (reverse seq))
        seq))

; (print (deep-reverse (list 2 (list 3 1) (list 7 (list 2 5 6) 5 1))))

; 2.28
(define (fringe seq)
    (cond
        ((null? seq) '())
        ((atom? seq) (list seq))
        (else (append (fringe (car seq)) (fringe (cdr seq))))))

; (print (fringe (list (list (list 1 2) 3) (list 2 3) 7 (list 1) 3 13)))

; 2.31
(define (tree-map fn tree)
    (map (lambda (sub-tree)
        (if (list? sub-tree)
            (tree-map fn sub-tree)
            (fn sub-tree))) tree))

; (print (tree-map square (list 1 2 (list 5 1) (list 3 7 (list 9 2)))))
