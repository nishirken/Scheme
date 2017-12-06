(load "utils/print.scm")
(load "utils/maths.scm")

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (dispatch m)
        (cond
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m))))
    dispatch)

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

; 3.3
(define (make-protected-account balance initial-password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (dispatch m password)
        (if (eq? password initial-password)
            (cond
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m)))
            (error "Password failed" password)))
    dispatch)

(define acc
    (make-protected-account 100 'qwerty))

; (print ((acc 'withdraw 'qwerty) 20))
; (print ((acc 'deposit 'qwerty) 30))
; (print ((acc 'withdraw 'qq) 30))

; 3.4
(define (make-protected-account-alert balance initial-password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (let ((invalid-password-counter 0))
        (lambda (m password)
            (cond
                ((eq? password initial-password)
                    (begin (set! invalid-password-counter 0)
                        (cond
                            ((eq? m 'withdraw) withdraw)
                            ((eq? m 'deposit) deposit)
                            (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m)))))
                ((= invalid-password-counter 4) (error "Password failed, We've called the police!!!"))
                (else (begin
                    (set! invalid-password-counter (inc invalid-password-counter))
                    (lambda (args) (list "Password failed" "Remained attemts: " (- 5 invalid-password-counter)))))))))

(define acc-alert
    (make-protected-account-alert 200 'qq))
; (print ((acc-alert 'withdraw 'qq) 20))
; (print ((acc-alert 'withdraw '1) 20))
; (print ((acc-alert 'withdraw '11) 20))
; (print ((acc-alert 'withdraw '1q) 20))
; (print ((acc-alert 'withdraw '1ee) 20))
; (print ((acc-alert 'withdraw '1a) 20))
