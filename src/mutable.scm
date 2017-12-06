(load "utils/print.scm")
(load "utils/maths.scm")

; 3.2
(define (make-monitored f)
    (let ((counter 0))
        (define (mf . args)
            (cond
                ((eq? (car args) 'how-many-calls?) counter)
                ((eq? (car args) 'reset-count) (begin (set! counter 0) counter))
                (else (begin (set! counter (inc counter)) (apply f args)))))
    mf))

(define s
    (make-monitored +))
; (print (s 'how-many-calls?))
; (print (s 1 2 3 1))
; (print (s 1 1))
; (print (s 'how-many-calls?))
; (print (s 'reset-count))
; (print (s 'how-many-calls?))
