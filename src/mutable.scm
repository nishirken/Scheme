(load "utils/print.scm")

; 3.1
(define (make-accumulator initial)
    (lambda (amount)
        (begin (set! initial (+ amount initial)) initial)))

(define A (make-accumulator 10))
(define B (make-accumulator 5))
; (print (A 5))
; (print (A 15))
; (print (B 9))
; (print (B 2))

; 3.2
